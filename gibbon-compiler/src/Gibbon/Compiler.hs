{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | The compiler pipeline, assembled from several passes.

module Gibbon.Compiler
    ( -- * Compiler entrypoints
      compile, compileCmd
      -- * Configuration options and parsing
     , Config (..), Mode(..), Input(..)
     , configParser, configWithArgs, defaultConfig
      -- * Some other helper fns
     , compileAndRunExe
    )
  where

import           Control.DeepSeq
import           Control.Exception
#if !MIN_VERSION_base(4,15,0)
import           Control.Monad (when)
#endif
import           Control.Monad.State.Strict
import           Control.Monad.Reader (ask)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error (isDoesNotExistError)
import           System.Process
import           Text.PrettyPrint.GenericPretty

import           Gibbon.Common
import           Gibbon.DynFlags
import           Gibbon.Language
import qualified Gibbon.HaskellFrontend as HS
import qualified Gibbon.L0.Syntax as L0
import qualified Gibbon.L1.Syntax as L1
import qualified Gibbon.L2.Syntax as L2
import qualified Gibbon.L4.Syntax as L4
import qualified Gibbon.SExpFrontend as SExp
import           Gibbon.L0.Interp()
import           Gibbon.L1.Interp()
import           Gibbon.L2.Interp ( Store, emptyStore )
-- import           Gibbon.TargetInterp (Val (..), execProg)

-- Compiler passes
import qualified Gibbon.L0.Typecheck as L0
import qualified Gibbon.L0.Specialize2 as L0
import qualified Gibbon.L1.Typecheck as L1
import qualified Gibbon.L2.Typecheck as L2
import qualified Gibbon.L3.Typecheck as L3
import           Gibbon.Passes.Freshen        (freshNames)
import           Gibbon.Passes.Flatten        (flattenL1, flattenL2, flattenL3)
import           Gibbon.Passes.InlineTriv     (inlineTriv)
import           Gibbon.Passes.Simplifier     (simplify)
-- import           Gibbon.Passes.Sequentialize  (sequentialize)

import           Gibbon.Passes.DirectL3       (directL3)
import           Gibbon.Passes.InferLocations (inferLocs)
-- import           Gibbon.Passes.RepairProgram  (repairProgram)
import           Gibbon.Passes.AddRAN         (addRAN,needsRAN)
import           Gibbon.Passes.AddTraversals  (addTraversals)
import           Gibbon.Passes.RemoveCopies   (removeCopies)
import           Gibbon.Passes.InferEffects   (inferEffects)
import           Gibbon.Passes.ParAlloc       (parAlloc)
import           Gibbon.Passes.InferRegionScope (inferRegScope)
import           Gibbon.Passes.RouteEnds      (routeEnds)
import           Gibbon.Passes.ThreadRegions  (threadRegions)
import           Gibbon.Passes.Cursorize      (cursorize)
import           Gibbon.Passes.FindWitnesses  (findWitnesses)
-- -- import           Gibbon.Passes.ShakeTree      (shakeTree)
import           Gibbon.Passes.HoistNewBuf    (hoistNewBuf)
import           Gibbon.Passes.Unariser       (unariser)
import           Gibbon.Passes.Lower          (lower)
import           Gibbon.Passes.FollowRedirects(followRedirects)
import           Gibbon.Passes.RearrangeFree  (rearrangeFree)
import           Gibbon.Passes.Codegen        (codegenProg)
import           Gibbon.Passes.Fusion2        (fusion2)
import           Gibbon.Pretty

#ifdef LLVM_ENABLED
import qualified Gibbon.Passes.LLVM.Codegen as LLVM
#endif



-- Configuring and launching the compiler.
--------------------------------------------------------------------------------

suppress_warnings :: String
-- suppress_warnings = " -Wno-int-to-pointer-cast -Wno-switch-bool -Wno-return-type "
suppress_warnings = " "

configParser :: Parser Config
configParser = Config <$> inputParser
                      <*> modeParser
                      <*> ((Just <$> strOption (long "bench-input" <> metavar "FILE" <>
                                      help ("Hard-code the input file for --bench-fun, otherwise it"++
                                            " becomes a command-line argument of the resulting binary."++
                                            " Also we RUN the benchmark right away if this is provided.")))
                          <|> pure Nothing)
                      <*> ((Just <$> strOption (long "array-input" <> metavar "FILE" <>
                                      help ("Hard-code the input file for readArrayFile or it"++
                                            " becomes a command-line argument of the resulting binary.")))
                          <|> pure Nothing)
                      <*> (option auto (short 'v' <> long "verbose" <>
                                       help "Set the debug output level, 1-5, mirrors DEBUG env var.")
                           <|> pure 1)
                      <*> ((strOption $ long "cc" <> help "Set C compiler, default 'gcc'")
                            <|> pure (cc defaultConfig))
                      <*> ((strOption $ long "optc" <> help "Set C compiler options, default '-std=gnu11 -O3'")
                           <|> pure (optc defaultConfig))
                      <*> ((fmap Just (strOption $ long "cfile" <> help "Set the destination file for generated C code"))
                           <|> pure (cfile defaultConfig))
                      <*> ((fmap Just (strOption $ short 'o' <> long "exefile" <>
                                       help "Set the destination file for the executable"))
                           <|> pure (exefile defaultConfig))
                      <*> backendParser
                      <*> dynflagsParser
                      <*> (Just <$> strOption hidden <|> pure Nothing)
 where
  inputParser :: Parser Input
                -- I'd like to display a separator and some more info.  How?
  inputParser = -- infoOption "foo" (help "bar") <*>
                flag' Haskell (long "hs")  <|>
                flag Unspecified SExpr (long "gib")

  modeParser = -- infoOption "foo" (help "bar") <*>
               flag' ToParse (long "parse" <> help "Only parse, then print & stop") <|>
               flag' ToC     (long "toC" <> help "Compile to a C file, named after the input") <|>



               flag' ToExe   (long "to-exe" <> help "Compile to a C file, named after the input") <|>
               flag' Interp1 (long "interp1" <> help "run through the interpreter early, right after parsing") <|>
               flag' Interp2 (short 'i' <> long "interp2" <>
                              help "Run through the interpreter after cursor insertion") <|>
               flag' RunExe  (short 'r' <> long "run"     <> help "Compile and then run executable") <|>
               (Bench <$> toVar <$> strOption (short 'b' <> long "bench-fun" <> metavar "FUN" <>
                                     help ("Generate code to benchmark a 1-argument FUN against a input packed file."++
                                           "  If --bench-input is provided, then the benchmark is run as well.")))

  -- use C as the default backend
  backendParser :: Parser Backend
  backendParser = flag C LLVM (long "llvm" <> help "use the llvm backend for compilation")


-- | Parse configuration as well as file arguments.
configWithArgs :: Parser (Config,[FilePath])
configWithArgs = (,) <$> configParser
                     <*> some (argument str (metavar "FILES..."
                                             <> help "Files to compile."))

--------------------------------------------------------------------------------

-- | Command line version of the compiler entrypoint.  Parses command
-- line arguments given as string inputs.  This also allows us to run
-- conveniently from within GHCI.  For example:
--
-- >  compileCmd $ words $ " -r -p -v5 examples/test11c_funrec.gib "
--
compileCmd :: [String] -> IO ()
compileCmd args = withArgs args $
    do (cfg,files) <- execParser opts
       case files of
         [f] -> compile cfg f
         _ -> do dbgPrintLn 1 $ "Compiling multiple files:  "++show files
                 mapM_ (compile cfg) files
  where
    opts = info (helper <*> configWithArgs)
      ( fullDesc
     <> progDesc "Compile FILES according to the below options."
     <> header "A compiler for a minature tree traversal language" )


sepline :: String
sepline = replicate 80 '='


data CompileState a = CompileState
    { cnt :: Int -- ^ Gensym counter
    , result :: Maybe (Value a) -- ^ Result of evaluating output of prior pass, if available.
    }

-- | Compiler entrypoint, given a full configuration and a list of
-- files to process, do the thing.
compile :: Config -> FilePath -> IO ()
compile config@Config{mode,input,verbosity,backend,cfile} fp0 = do
  -- set the env var DEBUG, to verbosity, when > 1
  setDebugEnvVar verbosity

  -- Use absolute path
  dir <- getCurrentDirectory
  let fp1 = dir </> fp0
  -- Parse the input file
  ((l0, cnt0), fp) <- parseInput config input fp1
  let config' = config { srcFile = Just fp }


  let initTypeChecked :: L0.Prog0
      initTypeChecked =
        -- We typecheck first to turn the appropriate VarE's into FunRefE's.
        fst $ runPassM defaultConfig cnt0
                (freshNames l0 >>=
                 (\fresh -> dbgTrace 5 ("\nFreshen:\n"++sepline++ "\n" ++pprender fresh) (L0.tcProg fresh)))

  case mode of
    Interp1 -> do
        dbgTrace passChatterLvl ("\nParsed:\n"++sepline++ "\n" ++ sdoc l0) (pure ())
        dbgTrace passChatterLvl ("\nTypechecked:\n"++sepline++ "\n" ++ pprender initTypeChecked) (pure ())
        runConf <- getRunConfig []
        (_s1,val,_stdout) <- gInterpProg () runConf initTypeChecked
        print val


    ToParse -> dbgPrintLn 0 $ pprender l0

    _ -> do
      dbgPrintLn passChatterLvl $
          " [compiler] pipeline starting, parsed program: "++
            if dbgLvl >= passChatterLvl+1
            then "\n"++sepline ++ "\n" ++ (sdoc l0)
            else show (length (sdoc l0)) ++ " characters."

      -- (Stage 1) Run the program through the interpreter
      initResult <- withPrintInterpProg initTypeChecked

      -- (Stage 2) C/LLVM codegen
      let outfile = getOutfile backend fp cfile

      -- run the initial program through the compiler pipeline
      let stM = passes config' l0
      l4  <- evalStateT stM (CompileState {cnt=cnt0, result=initResult})

      if mode == Interp2
      then do
        error "TODO: Interp2"
        -- l4res <- execProg l4
        -- mapM_ (\(IntVal v) -> liftIO $ print v) l4res
        -- exitSuccess
      else do
        str <- case backend of
                 C    -> codegenProg config' l4
#ifdef LLVM_ENABLED
                 LLVM -> LLVM.codegenProg True l4
#endif
                 LLVM -> error $ "Cannot execute through the LLVM backend. To build Gibbon with LLVM: "
                         ++ "stack build --flag gibbon:llvm_enabled"

        -- The C code is long, so put this at a higher verbosity level.
        dbgPrint passChatterLvl $ " [compiler] Final C codegen: " ++show (length str) ++" characters."
        dbgPrintLn 4 $ sepline ++ "\n" ++ str

        clearFile outfile
        writeFile outfile str

        -- (Stage 3) Code written, now compile if warranted.
        when (mode == ToExe || mode == RunExe || isBench mode ) $ do
          compileAndRunExe config fp >>= putStr
          return ()

runL0 :: L0.Prog0 -> IO ()
runL0 l0 = do
    -- FIXME: no command line option atm.  Just env vars.
    runConf <- getRunConfig []
    dbgPrintLn 2 $ "Running the following through L0.Interp:\n "++sepline ++ "\n" ++ sdoc l0
    execAndPrint () runConf l0
    exitSuccess

-- | The compiler's policy for running/printing L1 programs.
runL1 :: L1.Prog1 -> IO ()
runL1 l1 = do
    -- FIXME: no command line option atm.  Just env vars.
    runConf <- getRunConfig []
    dbgPrintLn 2 $ "Running the following through L1.Interp:\n "++sepline ++ "\n" ++ sdoc l1
    execAndPrint () runConf l1
    exitSuccess

-- | The compiler's policy for running/printing L2 programs.
runL2 :: L2.Prog2 -> IO ()
runL2 l2 = runL1 (L2.revertToL1 l2)

-- | Set the env var DEBUG, to verbosity, when > 1
-- TERRIBLE HACK!!
-- This verbosity value is global, "pure" and can be read anywhere
--
setDebugEnvVar :: Int -> IO ()
setDebugEnvVar verbosity =
  when (verbosity > 1) $ do
    setEnv "GIBBON_DEBUG" (show verbosity)
    l <- evaluate dbgLvl
    hPutStrLn stderr$ " ! We set DEBUG based on command-line verbose arg: "++show l


parseInput :: Config -> Input -> FilePath -> IO ((L0.Prog0, Int), FilePath)
parseInput cfg ip fp = do
  (l0, f) <-
    case ip of
      Haskell -> (, fp) <$> HS.parseFile cfg fp
      SExpr   -> (, fp) <$> SExp.parseFile fp
      Unspecified ->
        case takeExtension fp of
          ".hs"   -> (, fp) <$> HS.parseFile cfg fp
          ".sexp" -> (, fp) <$> SExp.parseFile fp
          ".rkt"  -> (, fp) <$> SExp.parseFile fp
          ".gib"  -> (, fp) <$> SExp.parseFile fp
          oth -> do
            -- A silly hack just out of sheer laziness vis-a-vis tab completion:
            let f1 = fp ++ ".gib"
                f2 = fp ++ "gib"
            f1' <- doesFileExist f1
            f2' <- doesFileExist f2
            if f1' && oth == ""
              then (,f2) <$> SExp.parseFile f1
              else if f2' && oth == "."
                     then (,f2) <$> SExp.parseFile f1
                     else error$ "compile: unrecognized file extension: "++
                          show oth++"  Please specify compile input format."
  let l0' = do parsed <- l0
               -- dbgTraceIt (sdoc parsed) (pure ())
               HS.desugarLinearExts parsed
  (l0'', cnt) <- pure $ runPassM defaultConfig 0 l0'
  pure ((l0'', cnt), f)


-- |
withPrintInterpProg :: L0.Prog0 -> IO (Maybe (Value L0.Exp0))
withPrintInterpProg l0 =
  if dbgLvl >= interpDbgLevel
  then do
    -- FIXME: no command line option atm.  Just env vars.
    runConf <- getRunConfig []
    (_s1,val,_stdout) <- gInterpProg () runConf l0
    dbgPrintLn interpDbgLevel $ " [eval] Init prog evaluated to: "++show val
    return $ Just val
  else
    return Nothing

-- | Compile and run the generated code if appropriate
--
compileAndRunExe :: Config -> FilePath -> IO String
compileAndRunExe cfg@Config{backend,arrayInput,benchInput,mode,cfile,exefile} fp = do
  exepath <- makeAbsolute exe
  clearFile exepath

  -- (Stage 4) Codegen finished, generate a binary
  dbgPrintLn minChatLvl cmd
  cd <- system cmd
  case cd of
    ExitFailure n -> error$ (show backend) ++" compiler failed!  Code: "++show n
    ExitSuccess -> do
      when (mode == ToExe) exitSuccess
      -- (Stage 5) Binary compiled, run if appropriate
      let runExe extra = do
            (_,Just hout,_, phandle) <- createProcess (shell (exepath++extra))
                                                 { std_out = CreatePipe }
            exitCode <- waitForProcess phandle
            case exitCode of
                ExitSuccess   -> hGetContents hout
                ExitFailure n -> die$ "Treelang program exited with error code  "++ show n

      runConf <- getRunConfig [] -- FIXME: no command line option atm.  Just env vars.
      case benchInput of
        -- CONVENTION: In benchmark mode we expect the generated executable to take 2 extra params:
        Just _ | isBench mode   -> case arrayInput of
                                     Nothing -> runExe $ " " ++ show (rcSize runConf) ++ " " ++ show (rcIters runConf)
                                     Just fp -> runExe $ " " ++ "--array-input " ++ fp ++ " "  ++ show (rcSize runConf) ++ " " ++ show (rcIters runConf)
        _      | mode == RunExe -> case arrayInput of
                                     Nothing -> runExe ""
                                     Just fp -> runExe $ " " ++ "--array-input " ++ fp ++ " "
        _                                -> return ""
  where outfile = getOutfile backend fp cfile
        exe = getExeFile backend fp exefile
        pointer = gopt Opt_Pointer $ dynflags cfg
        links = if pointer then " -lgc " else ""
        cmd = compilationCmd backend cfg ++ outfile ++ links ++ " -o " ++ exe ++ " -lm"


-- | Return the correct filename to store the generated code,
-- based on the backend used, and override options specified
--
getOutfile :: Backend -> FilePath -> Maybe FilePath -> FilePath
getOutfile _ _ (Just override) = override
getOutfile backend fp Nothing =
  replaceExtension fp $
    case backend of
      C    -> ".c"
      LLVM -> ".ll"

-- | Return the correct filename for the generated exe,
-- based on the backend used, and override options specified
--
getExeFile :: Backend -> FilePath -> Maybe FilePath -> FilePath
getExeFile _ _ (Just override) = override
getExeFile backend fp Nothing =
  let fp' = case backend of
               C -> fp
               LLVM -> replaceFileName fp ((takeBaseName fp) ++ "_llvm")
  in replaceExtension fp' ".exe"

-- | Compilation command
--
compilationCmd :: Backend -> Config -> String
compilationCmd LLVM _   = "clang-5.0 lib.o "
compilationCmd C config = (cc config) ++" -std=gnu11 "
                          ++(if bumpAlloc then "-D_BUMPALLOC " else " ")
                          ++(if pointer then "-D_POINTER " else " ")
                          ++(if parallel then "-fcilkplus -D_PARALLEL " else " ")
                          ++(optc config)++"  "
                          ++(if warnc then "-Wno-unused-variable -Wall " else suppress_warnings)
  where dflags = dynflags config
        bumpAlloc = gopt Opt_BumpAlloc dflags
        pointer = gopt Opt_Pointer dflags
        warnc = gopt Opt_Warnc dflags
        parallel = gopt Opt_Parallel dflags

-- |
isBench :: Mode -> Bool
isBench (Bench _) = True
isBench _ = False

-- | The debug level at which we start to call the interpreter on the program during compilation.
interpDbgLevel :: Int
interpDbgLevel = 5

-- |
clearFile :: FilePath -> IO ()
clearFile fileName = removeFile fileName `catch` handleErr
  where
   handleErr e | isDoesNotExistError e = return ()
               | otherwise = throwIO e

--------------------------------------------------------------------------------

-- | Replace the main function with benchmark code
--
benchMainExp :: L1.Prog1 -> PassM L1.Prog1
benchMainExp l1 = do
  Config{benchInput,dynflags,mode} <- ask
  case mode of
    Bench fnname -> do
      let tmp = "bnch"
          ([arg@(L1.PackedTy tyc _)], ret) = L1.getFunTy fnname l1
          -- At L1, we assume ReadPackedFile has a single return value:
          newExp = L1.TimeIt (
                        (L1.LetE (toVar tmp, [],
                                 arg,
                                 L1.PrimAppE
                                 (L1.ReadPackedFile benchInput tyc Nothing arg) [])
                        $ L1.LetE (toVar "benchres", [],
                                      ret,
                                      (L1.AppE fnname [] [L1.VarE (toVar tmp)]))
                        $
                        -- FIXME: should actually return the result,
                        -- as soon as we are able to print it.
                        (if (gopt Opt_BenchPrint dynflags)
                         then L1.VarE (toVar "benchres")
                         else L1.PrimAppE L1.MkTrue []))
                   ) ret True
      -- Initialize the main expression with a void type. The typechecker will fix it later.
      return $ l1{ L1.mainExp = Just $ (newExp, L1.voidTy) }
    _ -> return l1

-- | The main compiler pipeline
passes :: (Show v) => Config -> L0.Prog0 -> StateT (CompileState v) IO L4.Prog
passes config@Config{dynflags} l0 = do
      let isPacked   = gopt Opt_Packed dynflags
          biginf     = gopt Opt_BigInfiniteRegions dynflags
          gibbon1    = gopt Opt_Gibbon1 dynflags
          no_rcopies = gopt Opt_No_RemoveCopies dynflags
          parallel   = gopt Opt_Parallel dynflags
          should_fuse = gopt Opt_Fusion dynflags
      l0 <- go  "freshen"         freshNames            l0
      l0 <- goE0 "typecheck"       L0.tcProg             l0
      l0 <- goE0 "bindLambdas"     L0.bindLambdas       l0
      l0 <- goE0 "monomorphize"    L0.monomorphize      l0
      -- l0 <- goE0 "closureConvert"  L0.closureConvert    l0
      l0 <- goE0 "specLambdas"     L0.specLambdas       l0
      l0 <- goE0 "desugarL0"       L0.desugarL0         l0
      -- Note: L0 -> L1
      l1 <- goE0 "toL1"            (pure . L0.toL1)     l0

      l1 <- goE1 "typecheck"     L1.tcProg              l1
      -- If we are executing a benchmark, then we
      -- replace the main function with benchmark code:
      l1 <- goE1 "benchMainExp"  benchMainExp           l1
      l1 <- goE1 "typecheck"     L1.tcProg              l1
      l1 <- goE1 "simplify"      simplify               l1
      l1 <- goE1 "typecheck"     L1.tcProg              l1
      -- Check this after eliminating all dead functions.
      when (hasSpawnsProg l1 && not parallel) $
        error "To compile a program with parallelism, use --parallel."
      l1 <- goE1 "flatten"       flattenL1              l1
      l1 <- goE1 "simplify"      simplify               l1
      l1 <- goE1 "inlineTriv"    inlineTriv             l1
      l1 <- goE1 "typecheck"     L1.tcProg              l1
      l1 <- if should_fuse
          then goE1  "fusion2"   fusion2                l1
          else return l1
      l1 <- goE0 "typecheck"     L1.tcProg              l1

      -- Minimal haskell "backend".
      lift $ dumpIfSet config Opt_D_Dump_Hs (render $ pprintHsWithEnv l1)

      -- -- TODO: Write interpreters for L2 and L3
      l3 <- if isPacked
            then do
              -- TODO: push data contstructors under conditional
              -- branches before InferLocations.

              -- Note: L1 -> L2
              l2 <- goE2 "inferLocations"  inferLocs    l1
              l2 <- go   "L2.typecheck"    L2.tcProg    l2
              l2 <- goE2 "L2.flatten"      flattenL2    l2
              l2 <- go   "L2.typecheck"    L2.tcProg    l2
              l2 <- if gibbon1 || no_rcopies
                    then return l2
                    else do l2 <- go "removeCopies" removeCopies l2
                            go "L2.typecheck"       L2.tcProg    l2
              l2 <- goE2 "inferEffects" inferEffects  l2

{- Note [Repairing programs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We need a program analysis that decides whether a L2 program needs to be repaired.
Why ? Because, when we pattern match on a packed value, L2 assumes that *every*
variable in that pattern is accessible. However, this is not always true.
Consider the rightmost fn (which does not traverse it's input):

   (Node [(x, loc_x), (y, loc_y)] BODY)

Here, since the input is not traversed, we won't have an end-witness for x. And we
cannot access y without it. We need to fix such programs. Effectively, what we're
looking for in this analyis is if we can unpack all the pattern matched variables
in case expressions occurring in the program. For functions, it means that either
the function should traverse it's input, or the un-reachable elements in the pattern
match must remain unused (eg. 'leftmost'). On the other hand, we always have to
repair a broken main expression (since the "unused" case won't apply).

The compiler has access to 2 program repair strategies -- dummy traversals or
random access nodes. If we're operating in gibbon1 mode, it uses the former. However,
this changes the asymptotic complexity of the functions. In gibbon2 mode, we compile
such programs to store RAN's instead. This basically allows O(1) access to any element
of a data constructor.

Also see Note [Adding dummy traversals] and Note [Adding random access nodes].

-}
              l2 <-
                if gibbon1
                then do
                  l2 <- goE2 "addTraversals" addTraversals l2
                  l2 <- go "L2.typecheck"    L2.tcProg     l2
                  l2 <- goE2 "inferEffects2"  inferEffects l2
                  l2 <- go "L2.typecheck"    L2.tcProg     l2
                  l2 <- goE2 "repairProgram"  (pure . id)  l2
                  pure l2
                else do
                  let need = needsRAN l2
                  l1 <- goE1 "addRAN"        (addRAN need) l1
                  l1 <- go "L1.typecheck"    L1.tcProg     l1
                  l2 <- go "inferLocations2" inferLocs     l1
                  l2 <- go "L2.flatten"      flattenL2     l2
                  l2 <- go "findWitnesses" findWitnesses   l2
                  l2 <- go "L2.typecheck"    L2.tcProg     l2
                  l2 <- goE2 "L2.flatten"    flattenL2     l2
                  l2 <- go "L2.typecheck"    L2.tcProg     l2
                  l2 <- goE2 "removeCopies"  removeCopies  l2
                  l2 <- go "L2.typecheck"    L2.tcProg     l2
                  l2 <- goE2 "inferEffects2" inferEffects  l2
                  l2 <- go "L2.typecheck"    L2.tcProg     l2
                  l2 <- goE2 "addTraversals" addTraversals l2
                  l2 <- go "L2.typecheck"    L2.tcProg     l2
                  l2 <- goE2 "repairProgram" (pure . id)   l2
                  pure l2

              lift $ dumpIfSet config Opt_D_Dump_Repair (pprender l2)
              l2 <- if gopt Opt_Parallel dynflags
                    then do
                      l2 <- goE2 "parAlloc"   parAlloc   l2
                      lift $ dumpIfSet config Opt_D_Dump_ParAlloc (pprender l2)
                      l2 <- go "L2.typecheck" L2.tcProg  l2
                      pure l2
                    else (pure l2)
              l2 <- goE2 "inferRegScope"   inferRegScope l2
              l2 <- go "L2.typecheck"     L2.tcProg      l2
              l2 <- goE2 "routeEnds"       routeEnds     l2
              -- _ <- lift $ putStrLn (pprender l2)
              l2 <- go "L2.typecheck"     L2.tcProg     l2
              -- N.B ThreadRegions doesn't produce a type-correct L2 program --
              -- it adds regions to 'locs' in AppE and LetE which the
              -- typechecker doesn't know how to handle.
              l2 <- go "threadRegions"    threadRegions l2

              -- Note: L2 -> L3
              -- TODO: Compose L3.TcM with (ReaderT Config)
              l3 <- go "cursorize"        cursorize     l2
              -- _ <- lift $ putStrLn (pprender l3)
              l3 <- go "L3.flatten"       flattenL3     l3
              l3 <- go "L3.typecheck" (L3.tcProg isPacked) l3
              l3 <- go "hoistNewBuf"      hoistNewBuf   l3
              l3 <- go "L3.typecheck" (L3.tcProg isPacked) l3
              return l3
            else do
              l3 <- go "directL3"         directL3      l1
              l3 <- go "L3.typecheck" (L3.tcProg isPacked) l3
              return l3

      l3 <- go "unariser"       unariser                l3
      l3 <- go "L3.typecheck"   (L3.tcProg isPacked)    l3
      l3 <- go "L3.flatten"     flattenL3               l3
      l3 <- go "L3.typecheck"   (L3.tcProg isPacked)    l3

      -- Note: L3 -> L4
      l4 <- go "lower"          lower                   l3
      l4 <- if gibbon1 || not isPacked
            then do
              l4 <- go "rearrangeFree"   rearrangeFree   l4
              pure l4
            else do
              -- These additional case branches cause some tests in pointer mode to fail.
              l4 <- go "followRedirects" followRedirects l4
              l4 <- go "rearrangeFree"   rearrangeFree   l4
              -- l4 <- go "inlineTrivL4"    (pure . L4.inlineTrivL4) l4
              pure l4
      return l4
  where
      go :: PassRunner a b v
      go = pass config

      goE2 :: (InterpProg Store b, Show v) => InterpPassRunner a b Store v
      goE2 = passE emptyStore config

      goE0 :: (InterpProg () b, Show v) => InterpPassRunner a b () v
      goE0 = passE () config

      goE1 :: (InterpProg () b, Show v) => InterpPassRunner a b () v
      goE1 = passE () config

type PassRunner a b v = (Pretty b, Out b, NFData a, NFData b) =>
                         String -> (a -> PassM b) -> a -> StateT (CompileState v) IO b

type InterpPassRunner a b s v = (HasPretty a, HasPretty b, HasOut a, HasOut b,
                                HasGeneric a, HasGeneric b, HasNFData a, HasNFData b) =>
                                String -> (Prog a -> PassM (Prog b)) -> Prog a -> StateT (CompileState v) IO (Prog b)

-- | Run a pass and return the result
--
pass :: Config -> PassRunner a b v
pass config who fn x = do
  cs@CompileState{cnt} <- get
  x' <- if dbgLvl >= passChatterLvl
        then lift $ evaluate $ force x
        else return x
  lift$ dbgPrint passChatterLvl $ " [compiler] Running pass, " ++who

  let (y,cnt') = runPassM config cnt (fn x')
  put cs{cnt=cnt'}
  y' <- if dbgLvl >= passChatterLvl
        then lift $ evaluate $ force y
        else return y
  if dbgLvl >= passChatterLvl+1
     then lift$ dbgPrintLn (passChatterLvl+1) $ "Pass output:\n"++sepline++"\n"++ (pprender y')
     -- TODO: Switch to a node-count for size output (add to GenericOps):
     else lift$ dbgPrintLn passChatterLvl $ "   => "++ show (length (sdoc y')) ++ " characters output."
  return y'


passChatterLvl :: Int
passChatterLvl = 3


-- | Like 'pass', but also evaluates and checks the result.
--
passE :: (InterpProg s p2, Show v) => s -> Config -> InterpPassRunner p1 p2 s v
passE s config@Config{mode} = wrapInterp s mode (pass config)


-- | An alternative version that allows FAILURE while running
-- the interpreter part.
-- FINISHME! For now not interpreting.
--
passF :: Config -> PassRunner p1 p2 v
passF config = pass config


-- | Wrapper to enable running a pass AND interpreting the result.
--
wrapInterp :: (InterpProg s b, Show v)
           => s -> Mode -> InterpPassRunner a b s v -> InterpPassRunner a b s v
wrapInterp s mode pass who fn x =
  do CompileState{result} <- get
     p2 <- pass who fn x
     -- In benchmark mode we simply turn OFF the interpreter.
     -- This decision should be finer grained.
     when (dbgLvl >= interpDbgLevel && not (isBench mode)) $ lift $ do
       let Just res1 = result
       -- FIXME: no command line option atm.  Just env vars.
       runConf <- getRunConfig []
       let res2 = gInterpNoLogs s runConf p2
       res2' <- catch (evaluate (force res2))
                (\exn -> error $ "Exception while running interpreter on pass result:\n"++sepline++"\n"
                         ++ show (exn::SomeException) ++ "\n"++sepline++"\nProgram was: "++abbrv 300 p2)
       unless (show res1 == res2') $
         error $ "After pass "++who++", evaluating the program yielded the wrong answer.\nReceived:  "
         ++show res2'++"\nExpected:  "++show res1
       dbgPrintLn interpDbgLevel $ " [interp] answer after " ++ who ++ " was: "++ res2'
     return p2

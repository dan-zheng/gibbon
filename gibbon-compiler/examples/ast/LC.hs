{-# LANGUAGE ScopedTypeVariables #-}

-- Substitution-based lambda calculus benchmark
module LC where

-- import Control.DeepSeq
-- import Data.Time.Clock
--import Helpers

-- anything thing other than the last constuctor should be in a function call
-- or add a pass that fuse constructors with calls simple
--  F ( K ..) = find the matching case in F and replace it with the code and just put it there..
--q? does it matter what are we traversing?

-- Lambda calculus with inteter arithmetic

--------------------------------------------------------------------------------
-- Grammar

data Val = IntV Int
         | LamV IntJ Exp
         | ErrorV
  deriving Show

data Exp = LamE IntJ Exp
         | AppE Exp Exp
         | VarE IntJ
         | LitE Int
         | PlusE Exp Exp
         | LetE IntJ Exp Exp
  deriving Show

data Binds = NilBinds | ConsBinds Int Exp Binds
  deriving Show

data IntJ = ZeroJ | SuccJ IntJ
  deriving Show

data BoolJ = TBool | FBool
  deriving Show

isEquel :: IntJ -> IntJ -> BoolJ
isEquel a b =
  case a of
    ZeroJ -> isZero b
    SuccJ a' -> isEquelHelper b  a'

isZero :: IntJ -> BoolJ
isZero a = case a of
    ZeroJ -> TBool
    SuccJ  x -> FBool

isEquelHelper :: IntJ -> IntJ -> BoolJ
isEquelHelper  b a'  =
 case b of
    ZeroJ -> FBool
    SuccJ b' -> isEquel a' b'

plusJ :: IntJ -> IntJ -> IntJ
plusJ i j =
  case i of
    ZeroJ   -> j
    SuccJ k -> SuccJ (plusJ k j)

data Symbol = A| B| C| D |E |F |G


data Vars = ConsVars IntJ Vars | NilVars
  deriving Show

removeVar :: Vars -> IntJ -> Vars
removeVar ls i =
  case ls of
    NilVars        -> NilVars
    ConsVars j rst ->
      let rst' = removeVar rst i
      in removeVarHelper (isEquel i j) j rst'

removeVarHelper :: BoolJ -> IntJ -> Vars -> Vars
removeVarHelper b j ls =
  case b of
    TBool -> ls
    FBool -> ConsVars j ls

memqVar :: Vars -> IntJ -> BoolJ
memqVar ls i =
  case ls of
    NilVars -> FBool
    ConsVars j rst -> memqVarHelper (isEquel i j) rst i

memqVarHelper :: BoolJ -> Vars -> IntJ -> BoolJ
memqVarHelper b ls i =
  case b of
    TBool -> TBool
    FBool -> memqVar ls i

appendVars :: Vars -> Vars -> Vars
appendVars xs ys =
  case xs of
    NilVars -> ys
    ConsVars x rst -> ConsVars x (appendVars rst ys)

--------------------------------------------------------------------------------
-- Interpreter

interpExp :: Exp -> Val
interpExp e =
    case e of
      LamE s e -> LamV s e
      AppE e1 e2 -> interpApp (interpExp e1) e2
      VarE s -> ErrorV
      LitE i -> IntV i
      PlusE e1 e2 -> interpPlus  (interpExp e1) e2
      LetE s e1 e2 -> interpExp (substExp e2 s e1)

interpApp :: Val -> Exp -> Val
interpApp v e2 =
    case v of
      IntV i -> ErrorV
      LamV s e -> interpExp (substExp e s e2 )
      ErrorV -> ErrorV

interpPlus :: Val -> Exp -> Val
interpPlus v e2 =
    case v of
      LamV s e -> ErrorV
      IntV i -> addN (interpExp e2) i
      ErrorV -> ErrorV

--------------------------------------------------------------------------------
-- Substitution

freeVars' :: Exp -> Vars -> Vars
freeVars' e acc =
  case e of
    LamE s e1  ->
      let fv_e1 = freeVars' e1 acc
      in appendVars (removeVar fv_e1 s) acc
    AppE e1 e2 ->
      let fv_e1 = freeVars' e1 acc
          fv_e2 = freeVars' e2 acc
      in appendVars (appendVars fv_e1 fv_e2) acc
    VarE s -> ConsVars s acc
    LitE _ -> acc
    PlusE e1 e2 ->
      let fv_e1 = freeVars' e1 acc
          fv_e2 = freeVars' e2 acc
      in appendVars (appendVars fv_e1 fv_e2) acc
    LetE s e1 e2 ->
      let fv_e1 = freeVars' e1 acc
          fv_e2 = freeVars' e2 acc
      in appendVars (appendVars fv_e1 (removeVar fv_e2 s)) acc

freeVars :: Exp -> Vars
freeVars e = freeVars' e NilVars

-- A lame gensym which walks over an expression and returns a variable which
-- is *fresh* in that expression.
gensym' :: Exp -> IntJ -> IntJ
gensym' e acc =
  case e of
    -- SuccJ works around 0+0=0
    LamE s e1  -> gensym' e1 (SuccJ (plusJ s acc))
    AppE e1 e2 ->
      let w = gensym' e1 acc
      in gensym' e2 w
    VarE s -> SuccJ (plusJ s acc)
    LitE _ -> acc
    PlusE e1 e2 ->
      let w = gensym' e1 acc
      in gensym' e2 w
    LetE s e1 e2 ->
      let w = gensym' e1 acc
      in gensym' e2 (SuccJ (plusJ s w))

substLamExp1 :: BoolJ -> IntJ -> IntJ -> Exp -> Exp -> Exp
substLamExp1 b y from e1 to =
  case b of
    TBool -> LamE y e1
    FBool -> substLamExp2 (memqVar (freeVars to) y) y from e1 to

substLamExp2 :: BoolJ -> IntJ -> IntJ -> Exp -> Exp -> Exp
substLamExp2 b y from e1 to =
  case b of
    FBool -> LamE y (substExp e1 from to)
    -- alpha rename the lambda expression before substituting
    TBool ->
      let z  = gensym' to ZeroJ
          z1 = gensym' e1 z
      in substExp (LamE z1 (substExp e1 y (VarE z1))) from to

substExpHelper2 :: BoolJ -> Exp -> IntJ -> Exp
substExpHelper2 b to s  =
  case b of
      TBool -> to
      FBool -> VarE s

substLetExp1 :: BoolJ -> IntJ -> IntJ -> Exp -> Exp ->Exp->Exp
substLetExp1 b y from to e1 e2 =
  case b of
      TBool -> LetE y (substExp e1 from to) e2
      FBool ->
        let e1' = (substExp e1 from to)
        in substLetExp2 (memqVar (freeVars to) y) y from to e1' e2

substLetExp2 :: BoolJ -> IntJ -> IntJ -> Exp -> Exp ->Exp->Exp
substLetExp2 b y from to e1' e2 =
  case b of
    FBool -> LetE y e1' (substExp e2 from to)
    -- alpha rename the let expression before substituting
    TBool ->
      let z  = gensym' to ZeroJ
          z1 = gensym' e2 z
      in substExp (LetE z1 e1' (substExp e2 y (VarE z1))) from to

-- Substitute an expression in place of a variable
substExp :: Exp -> IntJ -> Exp -> Exp
substExp e from to  =
    case e of
      LamE y e1 -> substLamExp1 (isEquel from y) y from e1 to
      AppE e1 e2 -> AppE (substExp e1 from to ) (substExp e2 from to )
      VarE s -> substExpHelper2 (isEquel from s) to s
      LitE i -> LitE i
      PlusE e1 e2 -> PlusE (substExp e1 from to ) (substExp e2 from to )
      LetE y e1 e2 -> substLetExp1 (isEquel from y) y from to e1 e2

addN :: Val ->Int  -> Val
addN v i1  =
    case v of
      LamV s e1 -> ErrorV
      IntV i2 -> IntV (i1 + i2)
      ErrorV -> ErrorV
{-

testSubst1 :: Exp
testSubst1 = substExp (LamE varA (VarE varC)) varC (VarE varA)

testSubst2 :: Exp
testSubst2 = substExp (LamE varA (VarE varA)) varC (VarE varB)

testSubst3 :: Exp
testSubst3 = substExp (LetE varA (LitE 10) (VarE varC)) varC (VarE varA)

-}

--------------------------------------------------------------------------------
-- Constant propogation

constProp :: Exp -> Exp
constProp e =
    case e of
      LamE s e1 -> LamE s (constProp e1)
      AppE e1 e2 -> AppE (constProp e1) (constProp e2)
      VarE s -> VarE s
      LitE i -> LitE i
      PlusE e1 e2 -> PlusE (constProp e1) (constProp e2)
      LetE s e1 e2 -> constPropLet  e1 e2 s


constPropLet :: Exp -> Exp -> IntJ-> Exp
constPropLet e1 e2 s  =
    case e1 of
      LamE sl el -> LetE s (LamE  sl (constProp el)) (constProp e2)
      AppE ea1 ea2 -> LetE s (AppE  (constProp ea1) (constProp ea2)) (constProp e2)
      VarE sv -> substExp  e2 s (VarE sv)
      LitE i -> substExp e2 s (LitE i)
      PlusE e1 e2 -> PlusE (constProp e1) (constProp e2)
      LetE sl el1 el2 -> LetE s (constPropLet  el1 el2 sl) (constProp e2)

--------------------------------------------------------------------------------
-- Partial evaluation of plus
ptExp :: Exp -> Exp
ptExp e =
    case e of
      LamE s e1 -> LamE s (ptExp e1)
      AppE e1 e2 -> AppE (ptExp e1) (ptExp e2)
      VarE s -> VarE s
      LitE i -> LitE i
      PlusE e1 e2 -> ptPlus (ptExp e1) (ptExp e2)
      LetE s e1 e2 -> LetE s (ptExp e1) (ptExp e2)


ptPlus :: Exp -> Exp -> Exp
ptPlus e1 e2 =
    case e1 of
      LamE sl el1 -> PlusE (LamE   sl el1) e2
      AppE ea1 ea2 -> PlusE (AppE  ea1 ea2) e2
      VarE s -> PlusE (VarE    s) e2
      LitE i -> plPlusInner  e2 i
      PlusE ep1 ep2 -> PlusE (PlusE   ep1 ep2) e2
      LetE s el1 el2 -> PlusE (LetE   s el1 el2) e2

plPlusInner ::   Exp ->Int-> Exp
plPlusInner e2 i =
    case e2 of
      LamE sl el1 -> PlusE (LitE  i) (LamE sl el1)
      AppE ea1 ea2 -> PlusE (LitE  i) (AppE ea1 ea2)
      VarE s -> PlusE (LitE  i) (VarE s)
      LitE i2 -> LitE (i + i2)
      PlusE ep1 ep2 -> PlusE (LitE  i) (PlusE ep1 ep2)
      LetE s el1 el2 -> PlusE (LitE  i) (LetE s el1 el2)

--------------------------------------------------------------------------------
-- Dead code elimination

data ExpVars = ExpVars Exp Vars
  deriving Show

getExp :: ExpVars -> Exp
getExp evs =
  case evs of
    ExpVars e1 ls -> e1

getVars :: ExpVars -> Vars
getVars evs =
  case evs of
    ExpVars e1 ls -> ls

elimDeadBindings' :: Exp -> Vars -> ExpVars
elimDeadBindings' e acc =
  case e of
    LamE v bod ->
      let vsbod' = elimDeadBindings' bod acc
          vs     = getVars vsbod'
          bod'   = getExp vsbod'
          acc'   = appendVars vs acc
      in ExpVars (LamE v bod') acc'

    AppE a b ->
      let vsa' = elimDeadBindings' a acc
          vs   = getVars vsa'
          a'   = getExp vsa'
          wsb' = elimDeadBindings' b acc
          ws   = getVars wsb'
          b'   = getExp wsb'
      in ExpVars (AppE a' b') (appendVars (appendVars vs ws) acc)

    VarE v -> ExpVars (VarE v) (ConsVars v acc)

    LitE i -> ExpVars (LitE i) acc

    PlusE a b ->
      let vsa' = elimDeadBindings' a acc
          vs   = getVars vsa'
          a'   = getExp vsa'
          wsb' = elimDeadBindings' b acc
          ws   = getVars wsb'
          b'   = getExp wsb'
      in ExpVars (PlusE a' b') (appendVars (appendVars vs ws) acc)

    LetE v rhs bod ->
      let vsrhs' = elimDeadBindings' rhs acc
          vs     = getVars vsrhs'
          rhs'   = getExp vsrhs'
          wsbod' = elimDeadBindings' bod acc
          ws     = getVars wsbod'
          bod'   = getExp wsbod'
      in elimDeadBindingsLet' (memqVar ws v) v rhs' vs bod' ws acc

elimDeadBindingsLet' :: BoolJ -> IntJ -> Exp -> Vars -> Exp -> Vars -> Vars -> ExpVars
elimDeadBindingsLet' b v rhs vs bod ws acc =
  case b of
    -- drop this binding
    FBool -> ExpVars bod (appendVars ws acc)
    TBool -> ExpVars (LetE v rhs bod) (appendVars (appendVars vs ws) acc)

elimDeadBindings_helper :: ExpVars -> Exp
elimDeadBindings_helper evs =
 case evs of
        ExpVars e2 vs -> e2

elimDeadBindings :: Exp -> Exp
elimDeadBindings e =
 case e of
    LamE v bod ->
     let evs = elimDeadBindings' e NilVars in
     elimDeadBindings_helper evs
    AppE a b ->
      let evs = elimDeadBindings' e NilVars in
      elimDeadBindings_helper evs
    VarE v ->
      let evs = elimDeadBindings' e NilVars in
      elimDeadBindings_helper evs
    LitE i ->
      let evs = elimDeadBindings' e NilVars in
      elimDeadBindings_helper evs
    PlusE a b ->
      let evs = elimDeadBindings' e NilVars in
      elimDeadBindings_helper evs
    LetE v rhs bod ->
      let evs = elimDeadBindings' e NilVars in
      elimDeadBindings_helper evs


{-

testElimDeadBindings :: Exp
testElimDeadBindings = elimDeadBindings $
  LetE varA (LamE varC (PlusE (VarE varC) (LitE 10))) $
  LetE varB (AppE (VarE varA) (LitE 20)) $
  LetE varD (LamE varC (PlusE (VarE varC) (LitE 100))) $
  LetE varE (LitE 20) $
  VarE varB

-}

--------------------------------------------------------------------------------
-- Desugar let*, square and incr

desugarExp :: Exp -> Exp
desugarExp e =
  case e of
    LamE s e1 -> LamE s (ptExp e1)
    AppE e1 e2 -> AppE (ptExp e1) (ptExp e2)
    VarE s -> VarE s
    LitE i -> LitE i
    PlusE e1 e2 -> ptPlus (ptExp e1) (ptExp e2)
    LetE s e1 e2 -> LetE s (ptExp e1) (ptExp e2)


--------------------------------------------------------------------------------


-- Example term
varA :: IntJ
varA =  ZeroJ

varB :: IntJ
varB =  SuccJ varA

varC :: IntJ
varC =  SuccJ varB

varD :: IntJ
varD =  SuccJ varC

varE :: IntJ
varE =  SuccJ varD

varF :: IntJ
varF =  SuccJ varE

ex1 :: Exp
ex1 = (LetE (varA) (LitE 30)
                (LetE (varB) (LamE (varC) (PlusE (LitE 10) (VarE (varC))))
                      (AppE (VarE (varB))
                            (PlusE (VarE (varA)) (LitE 2)))))

-- buildLargeExp :: Int -> Exp
-- buildLargeExp n =
--   if (n== 0 )
--     then
--        ex1
--     else
--       LetE (ZeroJ) ex1 (buildLargeExp (n-1))

buildLargeExp :: Int -> Exp
buildLargeExp n225 =
    let fltIf598 = n225 == 0 in
    if fltIf598
    then ex1
    else    let fltPkd600   = ex1 in
            let fltAppE602 = n225 - 1 in
            let fltPkd601 = buildLargeExp fltAppE602 in
            (PlusE fltPkd601 fltPkd601 )


eval :: Val -> Int
eval v =
    case v of
      IntV i -> i
      LamV s e -> -1
      ErrorV -> -2

{-
main =
 do
  let fltAppE507 :: Exp = buildLargeExp 23
  t1 <- fltAppE507  `deepseq` getCurrentTime
  t2 <-  (gibbon_main fltAppE507) `deepseq` getCurrentTime
  print $ diffUTCTime t2 t1
-}

gibbon_main = (eval (interpExp (elimDeadBindings (ptExp (constProp (buildLargeExp 1))))))
--  gibbon_main =  (interpExp (ptExp (constProp (buildLargeExp 100))))

-- main :: IO ()
-- main = do
--   print (eval (interpExp (ptExp (constProp ex1))))

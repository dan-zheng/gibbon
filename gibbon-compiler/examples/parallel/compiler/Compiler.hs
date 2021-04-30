-- module Compiler where

import Gibbon.Prelude
import Gibbon.List

--------------------------------------------------------------------------------
-- Data types common to all languages

type Ty = Sym
type Label = Sym
type Var = Sym

data Arg = IntArg Int | TrueArg | FalseArg | VarArg Var
  deriving (Show, Generic, NFData)

data Prim = AddP | SubP | AndP | OrP
  deriving (Show, Generic, NFData)

data Cmp = EqP | LtP
  deriving (Show, Generic, NFData)

-- data Val = IntV Int | ErrorV
--   deriving (Show, Generic, NFData)

-- data Type = IntTy | BoolTy

--------------------------------------------------------------------------------
-- Environments

-- ----------------------------------------
-- -- UT hash based
-- ----------------------------------------

-- -- Map Sym Sym
-- type VarEnv = SymHash
-- type TypeEnv = SymHash
-- type AliasEnv = SymHash

-- empty_env :: VarEnv
-- empty_env = empty_hash

-- lookup_env :: VarEnv -> Var -> Var
-- lookup_env env k = lookup_hash env k

-- insert_env :: VarEnv -> Var -> Var -> VarEnv
-- insert_env env k v = insert_hash env k v

-- contains_env :: VarEnv -> Var -> Bool
-- contains_env env k = contains_hash env k

-- -- Map Sym Int
-- type HomesEnv = IntHash

-- empty_int_env :: HomesEnv
-- empty_int_env = empty_int_hash

-- lookup_int_env :: HomesEnv -> Var -> Int
-- lookup_int_env env k = lookup_int_hash env k

-- insert_int_env :: HomesEnv -> Var -> Int -> HomesEnv
-- insert_int_env env k v = insert_int_hash env k v

-- contains_int_env :: HomesEnv -> Var -> Bool
-- contains_int_env env k = contains_int_hash env k

----------------------------------------
-- List based
----------------------------------------

type TypeEnv = List (Sym, Sym)
type VarEnv = List (Sym, Sym)
type AliasEnv = List (Sym, Sym)

empty_env :: VarEnv
empty_env =
  let x :: VarEnv
      x = alloc_ll
  in x

insert_env :: VarEnv -> Var -> Var -> VarEnv
insert_env env k v = cons_ll (k,v) env

default_sym :: Sym
{-# INLINE default_sym #-}
default_sym = quote "notfound"

default_int :: Int
{-# INLINE default_int #-}
default_int = 0

lookup_env :: VarEnv -> Var -> Var
lookup_env env k = lookupWithDefault default_sym k env

contains_env :: VarEnv -> Var -> Bool
contains_env env k =
  let v = lookupWithDefault default_sym k env
  in if eqsym v default_sym
     then False
     else True

print_env :: VarEnv -> ()
print_env env =
  if is_empty_ll env
  then let _ = printsym (quote "\n")
       in ()
  else
    let (k1,v1) = head_ll env
        tl = (tail_ll env)
        _ = printsym (quote "(")
        _ = printsym k1
        _ = printsym (quote ",")
        _ = printsym v1
        _ = printsym (quote "), ")
    in print_env tl

lookupWithDefault :: b -> Sym -> List (Sym, b) -> b
lookupWithDefault def k env =
  if is_empty_ll env
  then def
  else
    let (k1,v1) = head_ll env
        tl = (tail_ll env)
    in if eqsym k k1
       then v1
       else lookupWithDefault def k tl


-- Map Sym Int
type HomesEnv = List (Sym, Int)

empty_int_env :: HomesEnv
empty_int_env =
  let x :: HomesEnv
      x = alloc_ll
  in x

insert_int_env :: HomesEnv -> Var -> Int -> HomesEnv
insert_int_env env k v = cons_ll (k,v) env

lookup_int_env :: HomesEnv -> Var -> Int
lookup_int_env env k = lookupWithDefault default_int k env

contains_int_env :: HomesEnv -> Var -> Bool
contains_int_env env k =
  let v = lookupWithDefault default_int k env
  in if v == default_int
     then False
     else True

--------------------------------------------------------------------------------


intTy :: Ty
{-# INLINE intTy #-}
intTy = quote "Int"

boolTy :: Ty
{-# INLINE boolTy #-}
boolTy = quote "Bool"

errorTy :: Ty
{-# INLINE errorTy #-}
errorTy = quote "Error"

eqTy :: Ty -> Ty -> Bool
{-# INLINE eqTy #-}
eqTy t1 t2 = eqsym t1 t2

--------------------------------------------------------------------------------
-- Copy, traverse, and print

copy_arg :: Arg -> Arg
copy_arg arg =
  case arg of
    IntArg i -> IntArg i
    TrueArg  -> TrueArg
    FalseArg -> FalseArg
    VarArg v -> VarArg v

trav_arg :: Arg -> Int
trav_arg arg =
  case arg of
    IntArg i -> 1
    TrueArg  -> 1
    FalseArg -> 1
    VarArg v -> 1

print_arg :: Arg -> ()
print_arg arg =
  case arg of
    IntArg i ->
      let _ = printsym (quote "(IntArg ")
          _ = printint i
          _ = printsym (quote ")")
      in ()
    TrueArg  -> printsym (quote "(TrueArg)")
    FalseArg -> printsym (quote "(FalseArg)")
    VarArg v ->
      let _ = printsym (quote "(VarArg ")
          _ = printsym v
          _ = printsym (quote ")")
      in ()

copy_prim :: Prim -> Prim
copy_prim p =
  case p of
    AddP -> AddP
    SubP -> SubP
    AndP -> AndP
    OrP  -> OrP

trav_prim :: Prim -> Int
trav_prim p =
  case p of
    AddP -> 0
    SubP -> 0
    AndP -> 0
    OrP  -> 0

print_prim :: Prim -> ()
print_prim p =
  case p of
    AddP -> printsym (quote "AddP")
    SubP -> printsym (quote "SubP")
    AndP -> printsym (quote "AndP")
    OrP  -> printsym (quote "OrP")

copy_cmp :: Cmp -> Cmp
copy_cmp c =
  case c of
    EqP  -> EqP
    LtP  -> LtP

trav_cmp :: Cmp -> Int
trav_cmp c =
  case c of
    EqP  -> 0
    LtP  -> 0

print_cmp :: Cmp -> ()
print_cmp c =
  case c of
    EqP  -> printsym (quote "EqP")
    LtP  -> printsym (quote "LtP")

-- print_val :: Val -> ()
-- print_val v =
--   case v of
--     IntV i ->
--       let _ = printsym (quote "IntV ")
--           _ = printint i
--       in ()
--     ErrorV -> printsym (quote "ErrorV")

--------------------------------------------------------------------------------
-- Source

data ExpR = ArgR Arg | ReadR | NegR ExpR | NotR ExpR
          | PrimR Prim ExpR ExpR | CmpR Cmp ExpR ExpR
          | LetR Var ExpR ExpR | IfR ExpR ExpR ExpR
  deriving (Show, Generic, NFData)

data R = ProgramR Ty ExpR
       | ErrorR Ty
  deriving (Show, Generic, NFData)

--------------------------------------------------------------------------------
-- ANF'd Source

data SimplExpA = ArgA Arg | ReadA | NegA Arg | NotA Arg
               | PrimA Prim Arg Arg | CmpA Cmp Arg Arg
  deriving (Show, Generic, NFData)

data ExpA = SimplA SimplExpA
          | LetA Var SimplExpA ExpA
          | LetA2 Var SimplExpA ExpA -- A let binding which does not have a conditional in its body.
          | IfA SimplExpA ExpA ExpA
  deriving (Show, Generic, NFData)

data A = ProgramA Ty ExpA
       | ErrorA Ty
  deriving (Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Copy, traverse, and print

trav_simpl_expa :: SimplExpA -> Int
trav_simpl_expa exp =
  case exp of
    ArgA a -> trav_arg a
    ReadA -> 1
    NegA a -> trav_arg a
    NotA a -> trav_arg a
    PrimA p a1 a2 ->
      let _ = trav_prim p
          _ = trav_arg a1
          _ = trav_arg a2
      in 1
    CmpA c a1 a2 ->
      let _ = trav_cmp c
          _ = trav_arg a1
          _ = trav_arg a2
      in 1


print_simpl_expa :: SimplExpA -> ()
print_simpl_expa exp =
  case exp of
    ArgA arg ->
      let _ = printsym (quote "(ArgA ")
          _ = print_arg arg
          _ = printsym (quote ")")
      in ()
    ReadA -> printsym (quote "(ReadA)")
    NegA arg ->
      let _ = printsym (quote "(NegA ")
          _ = print_arg arg
          _ = printsym (quote ")")
      in ()
    NotA arg ->
      let _ = printsym (quote "(NotA ")
          _ = print_arg arg
          _ = printsym (quote ")")
      in ()
    PrimA p a1 a2 ->
      let _ = printsym (quote "(PrimA ")
          _ = print_prim p
          _ = printsym (quote " ")
          _ = print_arg a1
          _ = printsym (quote " ")
          _ = print_arg a2
          _ = printsym (quote ")")
      in ()
    CmpA c a1 a2 ->
      let _ = printsym (quote "(CmpA ")
          _ = print_cmp c
          _ = printsym (quote " ")
          _ = print_arg a1
          _ = printsym (quote " ")
          _ = print_arg a2
          _ = printsym (quote ")")
      in ()

print_expa :: ExpA -> ()
print_expa exp =
  case exp of
    SimplA simpl ->
      let _ = printsym (quote "(SimplA ")
          _ = print_simpl_expa simpl
          _ = printsym (quote ")")
      in ()
    LetA v rhs bod ->
      let _ = printsym (quote "(LetA ")
          _ = printsym v
          _ = printsym (quote " ")
          _ = print_simpl_expa rhs
          _ = printsym (quote " ")
          _ = print_expa bod
          _ = printsym (quote ")")
      in ()
    LetA2 v rhs bod ->
      let _ = printsym (quote "(LetA2 ")
          _ = printsym v
          _ = printsym (quote " ")
          _ = print_simpl_expa rhs
          _ = printsym (quote " ")
          _ = print_expa bod
          _ = printsym (quote ")")
      in ()
    IfA a b c ->
      let _ = printsym (quote "(IfA ")
          _ = print_simpl_expa a
          _ = printsym (quote " ")
          _ = print_expa b
          _ = printsym (quote " ")
          _ = print_expa c
          _ = printsym (quote ")")
      in ()

print_program_a :: A -> ()
print_program_a prg =
  case prg of
    ProgramA ty exp ->
      let _ = printsym (quote "(ProgramA ")
          _ = printsym ty
          _ = printsym (quote " ")
          _ = print_expa exp
          _ = printsym (quote ")")
      in ()
    ErrorA err ->
      let _ = printsym (quote "(ErrorA")
          _ = printsym err
          _ = printsym (quote ")")
      in ()

--------------------------------------------------------------------------------
-- C

data ExpC = ArgC Arg | ReadC | NegC Arg | NotC Arg
          | PrimC Prim Arg Arg | CmpC Cmp Arg Arg
  deriving (Show, Generic, NFData)

data StmC = AssignC Var ExpC
  deriving (Show, Generic, NFData)

data TailC = RetC ExpC | SeqC StmC TailC
           | IfC Label Label ExpC
           | GotoC Label
  deriving (Show, Generic, NFData)

data BlkC = BlockCons Label TailC BlkC
          | BlockNil
          | BlockAppend BlkC BlkC
  deriving (Show, Generic, NFData)

data TailAndBlk = MkTailAndBlk TailC BlkC

data C = ProgramC Ty (List Sym) BlkC
       | ErrorC Ty
  deriving (Show, Generic, NFData)


--------------------------------------------------------------------------------
-- Copy, traverse, and print

copy_expc :: ExpC -> ExpC
copy_expc exp =
  case exp of
    ArgC arg -> ArgC (copy_arg arg)
    ReadC    -> ReadC
    NegC arg -> NegC (copy_arg arg)
    NotC arg -> NotC (copy_arg arg)
    PrimC p a1 a2 -> PrimC (copy_prim p) (copy_arg a1) (copy_arg a2)
    CmpC c a1 a2 -> CmpC (copy_cmp c) (copy_arg a1) (copy_arg a2)

trav_expc :: ExpC -> Int
trav_expc exp =
  case exp of
    ArgC arg -> trav_arg arg
    ReadC    -> 1
    NegC arg -> trav_arg arg
    NotC arg -> trav_arg arg
    PrimC p a1 a2 ->
      let _ = trav_prim p
          _ = trav_arg a1
          _ = trav_arg a2
      in 1
    CmpC c a1 a2 ->
      let _ = trav_cmp c
          _ = trav_arg a1
          _ = trav_arg a2
      in 1

copy_stm :: StmC -> StmC
copy_stm stm =
  case stm of
    AssignC v exp -> AssignC v (copy_expc exp)

trav_stm :: StmC -> Int
trav_stm stm =
  case stm of
    AssignC v exp -> trav_expc exp

copy_tail :: TailC -> TailC
copy_tail tail =
  case tail of
    RetC exp -> RetC (copy_expc exp)
    SeqC stm tail -> SeqC (copy_stm stm) (copy_tail tail)
    IfC thn els cmp -> IfC thn els (copy_expc cmp)
    GotoC lbl -> GotoC lbl

_copy_tail :: TailC -> TailC
_copy_tail tail =
  case tail of
    RetC exp -> RetC (copy_expc exp)
    SeqC stm tail -> SeqC (copy_stm stm) (copy_tail tail)
    IfC thn els cmp -> IfC thn els (copy_expc cmp)
    GotoC lbl -> GotoC lbl

trav_tail :: TailC -> Int
trav_tail tail =
  case tail of
    RetC exp ->
      trav_expc exp
    SeqC stm tail ->
      let _ = trav_stm stm
          _ = trav_tail tail
      in 1
    IfC thn els cmp ->
      trav_expc cmp
    GotoC lbl ->
      1

copy_blk :: BlkC -> BlkC
copy_blk blk =
  case blk of
    BlockCons lbl tail rst -> BlockCons lbl (copy_tail tail) (copy_blk rst)
    BlockNil -> BlockNil
    BlockAppend b1 b2 -> BlockAppend (copy_blk b1) (copy_blk b2)

print_expc :: ExpC -> ()
print_expc exp =
  case exp of
    ArgC arg ->
      let _ = printsym (quote "(ArgC ")
          _ = print_arg arg
          _ = printsym (quote ")")
      in ()
    ReadC -> printsym (quote "(ReadC)")
    NegC arg ->
      let _ = printsym (quote "(NegC ")
          _ = print_arg arg
          _ = printsym (quote ")")
      in ()
    NotC arg ->
      let _ = printsym (quote "(NotC ")
          _ = print_arg arg
          _ = printsym (quote ")")
      in ()
    PrimC p a1 a2 ->
      let _ = printsym (quote "(PrimC ")
          _ = print_prim p
          _ = printsym (quote " ")
          _ = print_arg a1
          _ = printsym (quote " ")
          _ = print_arg a2
          _ = printsym (quote ")")
      in ()
    CmpC c a1 a2 ->
      let _ = printsym (quote "(CmpC ")
          _ = print_cmp c
          _ = printsym (quote " ")
          _ = print_arg a1
          _ = printsym (quote " ")
          _ = print_arg a2
          _ = printsym (quote ")")
      in ()

print_stm :: StmC -> ()
print_stm stm =
  case stm of
    AssignC v exp ->
      let _ = printsym (quote "(AssignC ")
          _ = printsym v
          _ = printsym (quote " ")
          _ = print_expc exp
          _ = printsym (quote ")")
      in ()

print_tail :: TailC -> ()
print_tail tail =
  case tail of
    RetC exp ->
      let _ = printsym (quote "(RetC ")
          _ = print_expc exp
          _ = printsym (quote ")")
      in ()
    SeqC stm tail ->
      let _ = printsym (quote "(SeqC ")
          _ = print_stm stm
          _ = printsym (quote " ")
          _ = print_tail tail
          _ = printsym (quote ")")
      in ()
    IfC thn els exp ->
      let _ = printsym (quote "(IfC ")
          _ = printsym thn
          _ = printsym (quote " ")
          _ = printsym els
          _ = printsym (quote " ")
          _ = print_expc exp
          _ = printsym (quote ")")
      in ()
    GotoC lbl ->
      let _ = printsym (quote "(GotoC ")
          _ = printsym lbl
          _ = printsym (quote ")")
      in ()

print_blk :: BlkC -> ()
print_blk blk =
  case blk of
    BlockCons lbl tail rst ->
      let _ = printsym (quote "(BlockCons ")
          _ = printsym lbl
          _ = printsym (quote " ")
          _ = print_tail tail
          _ = printsym (quote " ")
          _ = print_blk rst
          _ = printsym (quote ")")
      in ()
    BlockNil -> printsym (quote "(BlockNil)")
    BlockAppend b1 b2 ->
      let _ = printsym (quote "(BlockApend ")
          _ = printsym (quote " ")
          _ = print_blk b1
          _ = printsym (quote " ")
          _ = print_blk b2
          _ = printsym (quote ")")
      in ()

print_tail_and_blk :: TailAndBlk -> ()
print_tail_and_blk tlblk =
  case tlblk of
    MkTailAndBlk tail blk ->
      let _ = printsym (quote "(MkTailAndBlk ")
          _ = print_tail tail
          _ = printsym (quote " ")
          _ = print_blk blk
          _ = printsym (quote ")")
      in ()

print_program_c :: C -> ()
print_program_c prg =
  case prg of
    ProgramC ty locals blk ->
      let _ = printsym (quote "(ProgramC ")
          _ = printsym ty
          _ = printsym (quote " ")
          _ = printsym (quote "(locals ")
          _ = print_locals locals
          _ = printsym (quote ") ")
          _ = print_blk blk
          _ = printsym (quote ")")
      in ()
    ErrorC err ->
      let _ = printsym (quote "(ErrorC")
          _ = printsym err
          _ = printsym (quote ")")
      in ()

print_locals :: List Sym -> ()
print_locals ls =
  if is_empty_ll ls
  then ()
  else
    let hd = head_ll ls
        tl = tail_ll ls
        _ = printsym hd
        _ = printsym (quote " ")
    in print_locals tl

print_varenv :: List (Sym,Sym) -> ()
print_varenv ls =
  if is_empty_ll ls
  then ()
  else
    let (a,b) = head_ll ls
        tl = tail_ll ls
        _ = printsym (quote "(")
        _ = printsym a
        _ = printsym (quote ",")
        _ = printsym b
        _ = printsym (quote ")")
        _ = printsym (quote " ")
    in print_varenv tl

--------------------------------------------------------------------------------
-- X86 with variables

type Reg = Sym

data PseudoX86 = ProgramX86 Ty (List Sym) Instrs
               | ErrorX86 Ty
  deriving (Show, Generic, NFData)

data Instrs = InstrCons Instr Instrs
            | InstrNil
            | InstrAppend Instrs Instrs
  deriving (Show, Generic, NFData)

data Instr = AddQ ArgX86 ArgX86
           | SubQ ArgX86 ArgX86
           | NegQ ArgX86
           | XorQ ArgX86 ArgX86
           | SetEQ Reg
           | CmpQ ArgX86 ArgX86
           | MovQ ArgX86 ArgX86
           | MovzbQ ArgX86 ArgX86
           | JumpQ Label
           | JumpEQ Label
           | PushQ ArgX86
           | PopQ ArgX86
           | RetQ
  deriving (Show, Generic, NFData)

data ArgX86 = IntX86 Int
            | VarX86 Var
            | RegX86 Reg
            | DerefX86 Reg Int
  deriving (Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Copy, traverse, and print

print_pseudox86 :: PseudoX86 -> ()
print_pseudox86 prg =
  case prg of
    ProgramX86 ty locals instrs ->
      let _ = printsym (quote "(locals ")
          _ = print_locals locals
          _ = printsym (quote ")\n")
      in print_instrs instrs
    ErrorX86 err ->
      let _ = printsym (quote "ErrorX86: ")
          _ = printsym err
      in ()

print_instrs :: Instrs -> ()
print_instrs instrs =
  case instrs of
    InstrCons instr rst ->
      let _ = print_instr instr
          _ = print_newline()
          _ = print_instrs rst
      in ()
    InstrNil -> ()
    InstrAppend instrs1 instrs2 ->
      let _ = print_instrs instrs1
          _ = print_newline()
          _ = print_instrs instrs2
      in ()

print_instr :: Instr -> ()
print_instr instr =
  case instr of
    AddQ a1 a2 ->
      let _ = printsym (quote "addq ")
          _ = print_argx86 a1
          _ = printsym (quote ", ")
          _ = print_argx86 a2
      in ()
    SubQ a1 a2 ->
      let _ = printsym (quote "subq ")
          _ = print_argx86 a1
          _ = printsym (quote ", ")
          _ = print_argx86 a2
      in ()
    NegQ a1 ->
      let _ = printsym (quote "negq ")
          _ = print_argx86 a1
      in ()
    XorQ a1 a2 ->
      let _ = printsym (quote "xorq ")
          _ = print_argx86 a1
          _ = printsym (quote ", ")
          _ = print_argx86 a2
      in ()
    SetEQ r ->
      let _ = printsym (quote "sete ")
          _ = print_reg r
      in ()
    CmpQ a1 a2 ->
      let _ = printsym (quote "cmpq ")
          _ = print_argx86 a1
          _ = printsym (quote ", ")
          _ = print_argx86 a2
      in ()
    MovQ a1 a2 ->
      let _ = printsym (quote "movq ")
          _ = print_argx86 a1
          _ = printsym (quote ", ")
          _ = print_argx86 a2
      in ()
    MovzbQ a1 a2 ->
      let _ = printsym (quote "movzbq ")
          _ = print_argx86 a1
          _ = printsym (quote ", ")
          _ = print_argx86 a2
      in ()
    JumpQ lbl ->
      let _ = printsym (quote "jmp ")
          _ = printsym lbl
      in ()
    JumpEQ lbl ->
      let _ = printsym (quote "je ")
          _ = printsym lbl
      in ()
    PushQ a1 ->
      let _ = printsym (quote "pushq ")
          _ = print_argx86 a1
      in ()
    PopQ a1 ->
      let _ = printsym (quote "popq ")
          _ = print_argx86 a1
      in ()
    RetQ ->
      let _ = printsym (quote "retq ")
      in ()


print_argx86 :: ArgX86 -> ()
print_argx86 arg =
  case arg of
    IntX86 i ->
      let _ = printint i
      in ()
    VarX86 v ->
      let _ = printsym v
      in ()
    RegX86 r ->
      print_reg r
    DerefX86 reg int ->
      let _ = printint int
          _ = printsym (quote "(")
          _ = print_reg reg
          _ = printsym (quote ")")
      in ()

print_reg :: Reg -> ()
print_reg r =
  let _ = printsym (quote "%")
      _ = printsym r
  in ()

--------------------------------------------------------------------------------
-- Typecheck

typecheck :: R -> R
typecheck prg =
  case prg of
    ProgramR expected exp ->
      let actual = typecheckExp empty_env exp
      in if eqTy expected actual
         -- COPY: exp is copied (indirection)
         then ProgramR expected exp
         else ErrorR errorTy
    ErrorR err -> ErrorR err

typecheckExp :: TypeEnv -> ExpR -> Ty
typecheckExp ty_env exp =
  case exp of
    ArgR arg -> typecheckArg ty_env arg
    ReadR    -> intTy
    NegR exp1 ->
      let texp1 = typecheckExp ty_env exp1
      in if eqTy texp1 intTy
         then intTy
         else errorTy
    NotR exp1 ->
      let texp1 = typecheckExp ty_env exp1
      in if eqTy texp1 boolTy
         then boolTy
         else errorTy
    PrimR p e1 e2 ->
      let -- TRAVERSAL: single byte traversal. OK.
          _ = trav_prim p
          te1 = typecheckExp ty_env e1
          te2 = typecheckExp ty_env e2
      in typecheckPrim p te1 te2
    CmpR c e1 e2 ->
      let -- TRAVERSAL: single byte traversal. OK.
          _ = trav_cmp c
          te1 = typecheckExp ty_env e1
          te2 = typecheckExp ty_env e2
      in typecheckCmp c te1 te2
    LetR v rhs bod ->
      let ty = typecheckExp ty_env rhs
          ty_env' = insert_env ty_env v ty
      in typecheckExp ty_env' bod
    IfR a b c ->
      let t1 = typecheckExp ty_env a
          t2 = typecheckExp ty_env b
          t3 = typecheckExp ty_env c
      in if eqTy t1 boolTy
         then if eqTy t2 t3
              then t2
              else errorTy
         else errorTy

typecheckArg :: TypeEnv -> Arg -> Ty
typecheckArg ty_env arg =
    case arg of
    IntArg i -> intTy
    TrueArg  -> boolTy
    FalseArg -> boolTy
    VarArg v ->
      let ty = lookup_env ty_env v
      -- lookup_env returns the same symbol its given if it doesn't find it in the dictionary.
      -- So we do a little bit of error checking over here.
      in if eqTy ty v
         then errorTy
         else ty

typecheckPrim :: Prim -> Ty -> Ty -> Ty
typecheckPrim p ty1 ty2 =
  case p of
    AddP -> if eqTy ty1 intTy && eqTy ty2 intTy
            then intTy
            else errorTy
    SubP -> if eqTy ty1 intTy && eqTy ty2 intTy
            then intTy
            else errorTy
    AndP -> if eqTy ty1 boolTy && eqTy ty2 boolTy
            then boolTy
            else errorTy
    OrP -> if eqTy ty1 boolTy && eqTy ty2 boolTy
           then boolTy
           else errorTy

typecheckCmp :: Cmp -> Ty -> Ty -> Ty
typecheckCmp c ty1 ty2 =
  case c of
    LtP -> if eqTy ty1 intTy && eqTy ty2 intTy
           then boolTy
           else errorTy
    EqP -> if eqTy ty1 intTy && eqTy ty2 intTy
           then boolTy
           else errorTy

--------------------------------------------------------------------------------
-- Typecheck ANF'd

typecheckA :: A -> A
typecheckA prg =
  case prg of
    ProgramA expected exp ->
      let actual = typecheckExpA empty_env exp
      in if eqTy expected actual
         -- COPY: exp is copied (indirection)
         then ProgramA expected exp
         else ErrorA errorTy
    ErrorA err -> ErrorA err

typecheckA_par :: A -> A
typecheckA_par prg =
  case prg of
    ProgramA expected exp ->
      let actual = typecheckExpA_par 0 empty_env exp
      in if eqTy expected actual
         -- COPY: exp is copied (indirection)
         then ProgramA expected exp
         else ErrorA errorTy
    ErrorA err -> ErrorA err


typecheckExpA :: TypeEnv -> ExpA -> Ty
typecheckExpA ty_env exp =
  case exp of
    SimplA simpl -> typecheckSimplExpA ty_env simpl
    LetA v rhs bod ->
      let ty = typecheckSimplExpA ty_env rhs
          ty_env' = insert_env ty_env v ty
      in typecheckExpA ty_env' bod
    LetA2 v rhs bod ->
      let ty = typecheckSimplExpA ty_env rhs
          ty_env' = insert_env ty_env v ty
      in typecheckExpA ty_env' bod
    IfA a b c ->
      let t1 = typecheckSimplExpA ty_env a
          t2 = typecheckExpA ty_env b
          t3 = typecheckExpA ty_env c
      in if eqTy t1 boolTy
         then if eqTy t2 t3
              then t2
              else errorTy
         else errorTy

typecheckExpA_par :: Int -> TypeEnv -> ExpA -> Ty
typecheckExpA_par depth ty_env exp =
  -- if depth > 8 then typecheckExpA ty_env exp else
  case exp of
    SimplA simpl -> typecheckSimplExpA ty_env simpl
    LetA v rhs bod ->
      let ty = typecheckSimplExpA ty_env rhs
          ty_env' = insert_env ty_env v ty
      in typecheckExpA_par (depth+1) ty_env' bod
    -- Bottom out to sequential.
    LetA2 v rhs bod ->
      let ty = typecheckSimplExpA ty_env rhs
          ty_env' = insert_env ty_env v ty
      in typecheckExpA ty_env' bod
    IfA a b c ->
      let t1 = typecheckSimplExpA ty_env a
          -- TODO:
          -- (ty_env1, ty_env2) = fork_pdict ty_env
          ty_env1 = ty_env
          ty_env2 = ty_env
          t2 = spawn (typecheckExpA_par (depth+1) ty_env1 b)
          t3 = typecheckExpA_par (depth+1) ty_env2 c
          _ = sync
      in if eqTy t1 boolTy
         then if eqTy t2 t3
              then t2
              else errorTy
         else errorTy

typecheckSimplExpA :: TypeEnv -> SimplExpA -> Ty
typecheckSimplExpA ty_env exp =
  case exp of
    ArgA arg -> typecheckArg ty_env arg
    ReadA    -> intTy
    NegA exp1 ->
      let texp1 = typecheckArg ty_env exp1
      in if eqTy texp1 intTy
         then intTy
         else errorTy
    NotA exp1 ->
      let texp1 = typecheckArg ty_env exp1
      in if eqTy texp1 boolTy
         then boolTy
         else errorTy
    PrimA p e1 e2 ->
      let -- TRAVERSAL: single byte traversal. OK.
          _ = trav_prim p
          te1 = typecheckArg ty_env e1
          te2 = typecheckArg ty_env e2
      in typecheckPrim p te1 te2
    CmpA c e1 e2 ->
      let -- TRAVERSAL: single byte traversal. OK.
          _ = trav_cmp c
          te1 = typecheckArg ty_env e1
          te2 = typecheckArg ty_env e2
      in typecheckCmp c te1 te2

--------------------------------------------------------------------------------
-- Constant folding

-- foldConstants :: R -> R
-- foldConstants prg =
--   case prg of
--     ProgramR ty exp -> ProgramR ty (fcExp empty_env exp)
--     ErrorR err -> ErrorR err

-- fcExpA :: ExpA -> ExpA
-- fcExpA exp =
--   case exp of
--     SimplA simpl -> SimplA (uniqifySimplExpA simpl)
--     LetA v rhs bod ->
--       let rhs' = fcSimplExpA rhs
--           bod' = fcExpA var_env' bod
--       in LetA v rhs' bod'
--     LetA2 v rhs bod ->
--       let rhs' = fcSimplExpA rhs
--           bod' = fcExpA var_env' bod
--       in LetA2 v rhs' bod'
--     IfA a b c -> IfA (fcSimplExpA a) (fcExpA b) (fcExpA c)

-- fcSimplExpA :: SimplExpA -> SimplExpA
-- fcSimplExpA exp =
--   case exp of
--     ArgA arg -> ArgA (copy_arg arg)
--     ReadA -> ReadA
--     NegA e -> NegA (copy_arg e)
--     NotA e -> NotA (copy_arg e)
--     -- PrimA p e1 e2 ->
--     --     case e1 of
--     --         ArgA a ->
--     --             case a of
--     --                 IntArg i ->
--     --                     case e2 of
--     --                         ArgA b ->
--     --                             case b of
--     --                                 IntArg j ->
--     CmpA c e1 e2  -> CmpA (copy_cmp c) (uniqifyArg e1) (uniqifyArg e2)

--------------------------------------------------------------------------------
-- Uniqify

uniqify :: R -> R
uniqify prg =
  case prg of
    ProgramR ty exp -> ProgramR ty (uniqifyExp empty_env exp)
    ErrorR err -> ErrorR err

uniqifyExp :: VarEnv -> ExpR -> ExpR
uniqifyExp var_env exp =
  case exp of
    ArgR arg -> ArgR (uniqifyArg var_env arg)
    ReadR -> ReadR
    NegR e -> NegR (uniqifyExp var_env e)
    NotR e -> NotR (uniqifyExp var_env e)
    -- COPY: prim and cmp are copied (single byte data, so copying is good.)
    PrimR p e1 e2 -> PrimR (copy_prim p) (uniqifyExp var_env e1) (uniqifyExp var_env e2)
    CmpR c e1 e2  -> CmpR (copy_cmp c) (uniqifyExp var_env e1) (uniqifyExp var_env e2)
    LetR v rhs bod ->
      if contains_env var_env v
      then
        let rhs' = uniqifyExp var_env rhs
            v'   = gensym
            var_env' = insert_env var_env v v'
            bod' = uniqifyExp var_env' bod
        in LetR v' rhs' bod'
      else
        let rhs' = uniqifyExp var_env rhs
            var_env' = insert_env var_env v v
            bod' = uniqifyExp var_env' bod
        in LetR v rhs' bod'
    IfR a b c -> IfR (uniqifyExp var_env a) (uniqifyExp var_env b) (uniqifyExp var_env c)

uniqifyArg :: VarEnv -> Arg -> Arg
uniqifyArg var_env arg =
  case arg of
    IntArg i -> IntArg i
    TrueArg  -> TrueArg
    FalseArg -> FalseArg
    VarArg v -> VarArg (lookup_env var_env v)

--------------------------------------------------------------------------------
-- Uniqify ANF'd

uniqifyA :: A -> A
uniqifyA prg =
  case prg of
    ProgramA ty exp -> ProgramA ty (uniqifyExpA empty_env exp)
    ErrorA err -> ErrorA err

uniqifyA_par :: A -> A
uniqifyA_par prg =
  case prg of
    ProgramA ty exp -> ProgramA ty (uniqifyExpA_par empty_env exp)
    ErrorA err -> ErrorA err

uniqifyExpA :: VarEnv -> ExpA -> ExpA
uniqifyExpA var_env exp =
  case exp of
    SimplA simpl -> SimplA (uniqifySimplExpA var_env simpl)
    LetA v rhs bod ->
      {-
       - if contains_env var_env v
       - then
       -}
        let rhs' = uniqifySimplExpA var_env rhs
            v'   = gensym
            var_env' = insert_env var_env v v'
            bod' = uniqifyExpA var_env' bod
        in LetA v' rhs' bod'
      {-
       - else
       -   let rhs' = uniqifySimplExpA var_env rhs
       -       var_env' = insert_env var_env v v
       -       bod' = uniqifyExpA var_env' bod
       -   in LetA v rhs' bod'
       -}
    LetA2 v rhs bod ->
      {-
       - if contains_env var_env v
       - then
       -}
        let rhs' = uniqifySimplExpA var_env rhs
            v'   = gensym
            var_env' = insert_env var_env v v'
            bod' = uniqifyExpA var_env' bod
        in LetA2 v' rhs' bod'
      {-
       - else
       -   let rhs' = uniqifySimplExpA var_env rhs
       -       var_env' = insert_env var_env v v
       -       bod' = uniqifyExpA var_env' bod
       -   in LetA2 v rhs' bod'
       -}
    IfA a b c -> IfA (uniqifySimplExpA var_env a) (uniqifyExpA var_env b) (uniqifyExpA var_env c)

uniqifyExpA_par :: VarEnv -> ExpA -> ExpA
uniqifyExpA_par var_env exp =
  case exp of
    SimplA simpl -> SimplA (uniqifySimplExpA var_env simpl)
    LetA v rhs bod ->
      {-
       - if contains_env var_env v
       - then
       -}
        let rhs' = uniqifySimplExpA var_env rhs
            v'   = gensym
            var_env' = insert_env var_env v v'
            bod' = uniqifyExpA_par var_env' bod
        in LetA v' rhs' bod'
      {-
       - else
       -   let rhs' = uniqifySimplExpA var_env rhs
       -       var_env' = insert_env var_env v v
       -       bod' = uniqifyExpA_par var_env' bod
       -   in LetA v rhs' bod'
       -}
    LetA2 v rhs bod ->
      {-
       - if contains_env var_env v
       - then
       -}
        let rhs' = uniqifySimplExpA var_env rhs
            v'   = gensym
            var_env' = insert_env var_env v v'
            bod' = uniqifyExpA var_env' bod
        in LetA2 v' rhs' bod'
      {-
       - else
       -   let rhs' = uniqifySimplExpA var_env rhs
       -       var_env' = insert_env var_env v v
       -       bod' = uniqifyExpA var_env' bod
       -   in LetA2 v rhs' bod'
       -}
    IfA a b c ->
      let a' = (uniqifySimplExpA var_env a)
          -- TODO:
          -- (var_env1, var_env2) = fork_pdict var_env
          -- var_env1 = var_env
          -- var_env2 = var_env
          b' = spawn (uniqifyExpA_par var_env b)
          c' = (uniqifyExpA_par var_env c)
          _ = sync
      in IfA a' b' c'

uniqifySimplExpA :: VarEnv -> SimplExpA -> SimplExpA
uniqifySimplExpA var_env exp =
  case exp of
    ArgA arg -> ArgA (uniqifyArg var_env arg)
    ReadA -> ReadA
    NegA e -> NegA (uniqifyArg var_env e)
    NotA e -> NotA (uniqifyArg var_env e)
    -- COPY: prim and cmp are copied (single byte data, so copying is good.)
    PrimA p e1 e2 -> PrimA (copy_prim p) (uniqifyArg var_env e1) (uniqifyArg var_env e2)
    CmpA c e1 e2  -> CmpA (copy_cmp c) (uniqifyArg var_env e1) (uniqifyArg var_env e2)

--------------------------------------------------------------------------------
-- Lower to C

explicateControl :: A -> C
explicateControl prg =
  case prg of
    ProgramA ty exp ->
      let (locals, exp') = explicateTail exp
      in case exp' of
           MkTailAndBlk tail blk0 ->
             let start = gensym
                 -- COPY: tail and blk0 are copied (indirection)
                 tail' = _copy_tail tail
                 -- TRAVERSAL: forcing random access here triggers a InferLocations bug.
                 blk2 = BlockCons start tail' blk0
             in ProgramC ty locals blk2


explicateControl_par :: A -> C
explicateControl_par prg =
  case prg of
    ProgramA ty exp ->
      let (locals, exp') = explicateTail_par exp
      in case exp' of
           MkTailAndBlk tail blk0 ->
             let start = gensym
                 -- COPY: tail and blk0 are copied (indirection)
                 tail' = _copy_tail tail
                 -- TRAVERSAL: forcing random access here triggers a InferLocations bug.
                 blk2 = BlockCons start tail' blk0
             in ProgramC ty locals blk2


explicateTail :: ExpA -> (List Sym, TailAndBlk)
explicateTail exp =
  case exp of
    SimplA simpl ->
      let tb = MkTailAndBlk (RetC (toExpC simpl)) BlockNil
          locals :: List Sym
          locals = alloc_ll
      in (locals, tb)
    LetA2 v rhs bod ->
      let (locals, tail) = explicateTail2 exp
          tb = MkTailAndBlk tail BlockNil
      in (locals, tb)
    LetA v rhs bod ->
      let rhs' = toExpC rhs
          (locals, bod') = explicateTail bod
      in case bod' of
           MkTailAndBlk tl blk ->
             -- COPY: tl and blk are copied (indirection)
             let stm = AssignC v rhs'
                 -- TRAVERSAL: random access
                 -- _ = trav_tail tl
                 tail = SeqC stm tl
                 locals' = cons_ll v locals
             in (locals', MkTailAndBlk tail blk)
    IfA a b c ->
      let a' = toExpC a
          (locals1, b') = explicateTail b
          (locals2, c') = explicateTail c
          locals3 = append_ll locals1 locals2
          -- locals3 = locals1
      in
        case b' of
          MkTailAndBlk thn_tail thn_blocks ->
            case c' of
              MkTailAndBlk els_tail els_blocks ->
                let thn_label = gensym
                    els_label = gensym
                    tail' = IfC thn_label els_label a'

                    -- -- (1) use appendBlocks
                    -- thn_tail' = copy_tail thn_tail
                    -- els_tail' = copy_tail els_tail
                    -- blks0 = appendBlocks thn_blocks els_blocks
                    -- blks1 = BlockCons els_label els_tail' blks0
                    -- blks2 = BlockCons thn_label thn_tail' blks1

                    -- (2) create a tree using BlockAppend
                    -- COPY: thn_blocks and els_blocks is copied (indirection)
                    -- TRAVERSAL: random access
                    -- _ = trav_tail thn_tail
                    thn_tail' = _copy_tail thn_tail
                    blks0 = BlockCons thn_label thn_tail' thn_blocks
                    -- _ = trav_tail els_tail
                    els_tail' = _copy_tail els_tail
                    blks1 = BlockCons els_label els_tail' els_blocks
                    blks2 = BlockAppend blks0 blks1

                    tb = MkTailAndBlk tail' blks2
                in (locals3, tb)

explicateTail_par :: ExpA -> (List Sym, TailAndBlk)
explicateTail_par exp =
  case exp of
    SimplA simpl ->
      let tb = MkTailAndBlk (RetC (toExpC simpl)) BlockNil
          locals :: List Sym
          locals = alloc_ll
      in (locals, tb)
    LetA2 v rhs bod ->
      let (locals, tail) = explicateTail2 exp
          tb = MkTailAndBlk tail BlockNil
      in (locals, tb)
    LetA v rhs bod ->
      let rhs' = toExpC rhs
          (locals, bod') = explicateTail_par bod
      in case bod' of
           MkTailAndBlk tl blk ->
             -- COPY: tl and blk are copied (indirection)
             let stm = AssignC v rhs'
                 -- TRAVERSAL: random access
                 -- _ = trav_tail tl
                 tail = SeqC stm tl
                 locals' = cons_ll v locals
             in (locals', MkTailAndBlk tail blk)
    IfA a b c ->
      let a' = toExpC a
          tup1 = spawn (explicateTail_par b)
          tup2 = explicateTail_par c
          _ = sync
          (locals1, b') = tup1
          (locals2, c') = tup2
          locals3 = append_ll locals1 locals2
      in
        case b' of
          MkTailAndBlk thn_tail thn_blocks ->
            case c' of
              MkTailAndBlk els_tail els_blocks ->
                let thn_label = gensym
                    els_label = gensym
                    tail' = IfC thn_label els_label a'

                    -- -- (1) use appendBlocks
                    -- thn_tail' = copy_tail thn_tail
                    -- els_tail' = copy_tail els_tail
                    -- blks0 = appendBlocks thn_blocks els_blocks
                    -- blks1 = BlockCons els_label els_tail' blks0
                    -- blks2 = BlockCons thn_label thn_tail' blks1

                    -- (2) create a tree using BlockAppend
                    -- COPY: thn_blocks and els_blocks is copied (indirection)
                    -- TRAVERSAL: random access
                    -- _ = trav_tail thn_tail
                    thn_tail' = _copy_tail thn_tail
                    blks0 = BlockCons thn_label thn_tail' thn_blocks
                    -- _ = trav_tail els_tail
                    els_tail' = _copy_tail els_tail
                    blks1 = BlockCons els_label els_tail' els_blocks
                    blks2 = BlockAppend blks0 blks1

                    tb = MkTailAndBlk tail' blks2
                in (locals3, tb)


explicateTail2 :: ExpA -> (List Sym, TailC)
explicateTail2 exp =
  case exp of
    SimplA simpl ->
      let locals :: List Sym
          locals = alloc_ll
          tail = RetC (toExpC simpl)
      in (locals, tail)
    LetA2 v rhs bod ->
      let rhs' = toExpC rhs
          stm = AssignC v rhs'
          (locals, tail) = explicateTail2 bod
          locals' = cons_ll v locals
          tail' = SeqC stm tail
      in (locals', tail')


-- COPY: everything is copied (smallish data, so copying is good.)
toExpC :: SimplExpA -> ExpC
toExpC exp =
  case exp of
    ArgA arg -> ArgC (copy_arg arg)
    ReadA -> ReadC
    NegA arg -> NegC (copy_arg arg)
    NotA arg -> NotC (copy_arg arg)
    PrimA p a1 a2 -> PrimC (copy_prim p) (copy_arg a1) (copy_arg a2)
    CmpA c a1 a2 -> CmpC (copy_cmp c) (copy_arg a1) (copy_arg a2)


appendBlocks :: BlkC -> BlkC -> BlkC
appendBlocks b1 b2 =
  case b1 of
    -- COPY: b2 is copied (indirection)
    BlockNil -> b2
    -- COPY: tl is copied (indirection)
    BlockCons lbl tl rst ->
      let rst' = appendBlocks rst b2
      in BlockCons lbl tl rst'


force_random_access :: TailAndBlk -> BlkC
force_random_access tb =
  case tb of
    MkTailAndBlk tail blk -> blk

--------------------------------------------------------------------------------
-- Remove trivial jumps

optimizeJumps :: C -> C
optimizeJumps prg =
  case prg of
    ProgramC ty locals blk ->
      let env = empty_env
          trivials = collectTrivial env blk
      in ProgramC ty locals (replaceJumps trivials blk)
    ErrorC err -> ErrorC err

collectTrivial :: AliasEnv -> BlkC -> AliasEnv
collectTrivial env blk =
  case blk of
    BlockNil -> env
    BlockCons lbl tail rst ->
      -- TRAVERSAL: tail is traversed (random access)
      consIfTrivial lbl tail (collectTrivial env rst)
    BlockAppend b1 b2 ->
      -- TRAVERSAL: b1 is traversed (random access)
      let env1 = collectTrivial env b1
          env2 = collectTrivial env1 b2
      in env2

consIfTrivial :: Label -> TailC -> AliasEnv -> AliasEnv
consIfTrivial lbl tail env =
  case tail of
    RetC _     -> env
    SeqC _ _   -> env
    GotoC lbl2 -> insert_env env lbl lbl2
    IfC _ _ _  -> env

replaceJumps :: AliasEnv -> BlkC -> BlkC
replaceJumps env blk =
  case blk of
    BlockNil -> BlockNil
    BlockCons lbl tail rst ->
      BlockCons lbl (replaceJumpsTail env tail) (replaceJumps env rst)
    BlockAppend b1 b2 ->
      let b1' = replaceJumps env b1
          b2' = replaceJumps env b2
      in BlockAppend b1' b2'

replaceJumps_par :: AliasEnv -> BlkC -> BlkC
replaceJumps_par env blk =
  case blk of
    BlockNil -> BlockNil
    BlockCons lbl tail rst ->
      BlockCons lbl (replaceJumpsTail env tail) (replaceJumps env rst)
    BlockAppend b1 b2 ->
      let b1' = spawn (replaceJumps_par env b1)
          b2' = replaceJumps_par env b2
          _ = sync
      in BlockAppend b1' b2'

replaceJumpsTail :: AliasEnv -> TailC -> TailC
replaceJumpsTail env tail =
  case tail of
    -- COPY: e is copied (indirection)
    RetC e -> RetC e
    -- COPY: stm is copied (indirection)
    SeqC stm tail2 ->
      SeqC stm (replaceJumpsTail env tail2)
    -- COPY: cmp is copied (indirection)
    IfC thn els cmp ->
      let thn' = lookup_env env thn
          els' = lookup_env env els
      in IfC thn' els' cmp

--------------------------------------------------------------------------------
-- Lower to pseudo x86

selectInstrs :: C -> PseudoX86
selectInstrs prg =
  case prg of
    ProgramC ty locals blk ->
      ProgramX86 ty locals (selectInstrsBlk blk)
    ErrorC err -> ErrorX86 err


selectInstrs_par :: C -> PseudoX86
selectInstrs_par prg =
  case prg of
    ProgramC ty locals blk ->
      ProgramX86 ty locals (selectInstrsBlk_par blk)
    ErrorC err -> ErrorX86 err


selectInstrsBlk :: BlkC -> Instrs
selectInstrsBlk blk =
  case blk of
    BlockNil -> InstrNil
    BlockCons lbl tail rst ->
      -- -- TRAVERSAL: tail is traversed (random access)
      -- selectInstrsTail2 tail rst

      -- better for block level parallelism
      let instrs1 = selectInstrsTail tail
          instrs2 = selectInstrsBlk rst
      in InstrAppend instrs1 instrs2

    BlockAppend blk1 blk2 ->
      -- TRAVERSAL: blk1 is traversed (random access)
      let instrs1 = selectInstrsBlk blk1
          instrs2 = selectInstrsBlk blk2
      in InstrAppend instrs1 instrs2

selectInstrsBlk_par :: BlkC -> Instrs
selectInstrsBlk_par blk =
  case blk of
    BlockNil -> InstrNil
    BlockCons lbl tail rst ->
      -- -- TRAVERSAL: tail is traversed (random access)
      -- selectInstrsTail2 tail rst

      -- can parallelize this too. but a single block might not have enough work in it...
      let instrs1 = spawn (selectInstrsTail tail)
          instrs2 = selectInstrsBlk_par rst
          _ = sync
      in InstrAppend instrs1 instrs2

    BlockAppend blk1 blk2 ->
      -- TRAVERSAL: blk1 is traversed (random access)
      let instrs1 = spawn (selectInstrsBlk_par blk1)
          instrs2 = selectInstrsBlk_par blk2
          _ = sync
      in InstrAppend instrs1 instrs2

selectInstrsTail2 :: TailC -> BlkC -> Instrs
selectInstrsTail2 tail blk_rst =
  case tail of
    RetC exp ->
      case exp of
        ArgC arg ->
          let arg' = selectInstrsArg arg
              arg2 = (RegX86 (quote "rax"))
              instr1 = (MovQ arg' arg2)
              instrs_rst = selectInstrsBlk blk_rst
          in InstrCons instr1 instrs_rst
        NegC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (MovQ arg' (RegX86 (quote "rax")))
              instr2 = (NegQ (RegX86 (quote "rax")))
              instrs_rst = selectInstrsBlk blk_rst
          in InstrCons instr1 (InstrCons instr2 instrs_rst)
        NotC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (MovQ arg' (RegX86 (quote "rax")))
              instr2 = (XorQ (IntX86 1) (RegX86 (quote "rax")))
              instrs_rst = selectInstrsBlk blk_rst
          in InstrCons instr1 (InstrCons instr2 instrs_rst)
        PrimC p a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_prim p
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case p of
               AddP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rax")))
                     a2' = selectInstrsArg a2
                     instr2 = (AddQ a2' (RegX86 (quote "rax")))
                     instrs_rst = selectInstrsBlk blk_rst
                 in InstrCons instr1 (InstrCons instr2 instrs_rst)
               SubP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rax")))
                     a2' = selectInstrsArg a2
                     instr2 = (SubQ a2' (RegX86 (quote "rax")))
                     instrs_rst = selectInstrsBlk blk_rst
                 in InstrCons instr1 (InstrCons instr2 instrs_rst)
        CmpC c a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_cmp c
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case c of
               EqP ->
                 let a1' = selectInstrsArg a1
                     a2' = selectInstrsArg a2
                     instr1 = (CmpQ a1' a2')
                     instr2 = (SetEQ (quote "al"))
                     instr3 = (MovzbQ (RegX86 (quote "al")) (RegX86 (quote "rax")))
                     instrs_rst = selectInstrsBlk blk_rst
                 in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 instrs_rst))

    SeqC stm tail_rst ->
      -- TRAVERSAL: stm is traversed (random access)
      case stm of
        AssignC v exp ->
          case exp of
            ArgC arg ->
              let arg' = selectInstrsArg arg
                  instr1 = (MovQ arg' (VarX86 v))
                  instrs_rst = selectInstrsTail2 tail_rst blk_rst
              in InstrCons instr1 instrs_rst
            NegC arg ->
              let arg' = selectInstrsArg arg
                  instr1 = (MovQ arg' (VarX86 v))
                  instr2 = (NegQ (VarX86 v))
                  instrs_rst = selectInstrsTail2 tail_rst blk_rst
              in InstrCons instr1 (InstrCons instr2 instrs_rst)
            NotC arg ->
              let arg' = selectInstrsArg arg
                  instr1 = (MovQ arg' (VarX86 v))
                  instr2 = (XorQ (IntX86 1) (VarX86 v))
                  instrs_rst = selectInstrsTail2 tail_rst blk_rst
              in InstrCons instr1 (InstrCons instr2 instrs_rst)
            PrimC p a1 a2 ->
              let -- TRAVERSAL: smallish traversal. OK.
                  _ = trav_prim p
                  _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
              in case p of
                   AddP ->
                     let a1' = selectInstrsArg a1
                         instr1 = (MovQ a1' (VarX86 v))
                         a2' = selectInstrsArg a2
                         instr2 = (AddQ a2' (VarX86 v))
                         instrs_rst = selectInstrsTail2 tail_rst blk_rst
                     in InstrCons instr1 (InstrCons instr2 instrs_rst)
                   SubP ->
                     let a1' = selectInstrsArg a1
                         instr1 = (MovQ a1' (VarX86 v))
                         a2' = selectInstrsArg a2
                         instr2 = (SubQ a2' (VarX86 v))
                         instrs_rst = selectInstrsTail2 tail_rst blk_rst
                     in InstrCons instr1 (InstrCons instr2 instrs_rst)

            CmpC c a1 a2 ->
              let -- TRAVERSAL: smallish traversal. OK.
                  _ = trav_cmp c
                  _ = trav_arg a1
              in case c of
                   EqP ->
                     let a1' = selectInstrsArg a1
                         a2' = selectInstrsArg a2
                         instr1 = (CmpQ a1' a2')
                         instr2 = (SetEQ (quote "al"))
                         instr3 = (MovzbQ (RegX86 (quote "al")) (VarX86 v))
                         instrs_rst = selectInstrsTail2 tail_rst blk_rst
                     in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 instrs_rst))

    IfC thn els exp ->
      case exp of
        ArgC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (CmpQ arg' (IntX86 1))
              instr2 = (JumpEQ thn)
              instr3 = (JumpQ els)
              instrs_rst = selectInstrsBlk blk_rst
          in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 instrs_rst))
        NegC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (MovQ arg' (RegX86 (quote "rbx")))
              instr2 = (NegQ (RegX86 (quote "rbx")))
              instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
              instr4 = (JumpEQ thn)
              instr5 = (JumpQ els)
              instrs_rst = selectInstrsBlk blk_rst
          in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 instrs_rst))))
        NotC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (MovQ arg' (RegX86 (quote "rbx")))
              instr2 = (XorQ (IntX86 1) (RegX86 (quote "rbx")))
              instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
              instr4 = (JumpEQ thn)
              instr5 = (JumpQ els)
              instrs_rst = selectInstrsBlk blk_rst
          in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 instrs_rst))))
        PrimC p a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_prim p
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case p of
               AddP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rbx")))
                     a2' = selectInstrsArg a2
                     instr2 = (AddQ a2' (RegX86 (quote "rbx")))
                     instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
                     instr4 = (JumpEQ thn)
                     instr5 = (JumpQ els)
                     instrs_rst = selectInstrsBlk blk_rst
                 in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 instrs_rst))))
               SubP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rbx")))
                     a2' = selectInstrsArg a2
                     instr2 = (SubQ a2' (RegX86 (quote "rbx")))
                     instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
                     instr4 = (JumpEQ thn)
                     instr5 = (JumpQ els)
                     instrs_rst = selectInstrsBlk blk_rst
                 in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 instrs_rst))))
        CmpC c a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_cmp c
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case c of
               EqP ->
                 let a1' = selectInstrsArg a1
                     a2' = selectInstrsArg a2
                     instr1 = (CmpQ a1' a2')
                     instr2 = (SetEQ (quote "al"))
                     instr3 = (MovzbQ (RegX86 (quote "al")) (RegX86 (quote "rbx")))
                     instr4 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
                     instr5 = (JumpEQ thn)
                     instr6 = (JumpQ els)
                     instrs_rst = selectInstrsBlk blk_rst
                 in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 (InstrCons instr6 instrs_rst)))))

selectInstrsTail :: TailC -> Instrs
selectInstrsTail tail =
  case tail of
    RetC exp ->
      case exp of
        ArgC arg ->
          let arg' = selectInstrsArg arg
          in InstrCons (MovQ arg' (RegX86 (quote "rax"))) InstrNil
        NegC arg ->
          let arg' = selectInstrsArg arg
          in InstrCons (MovQ arg' (RegX86 (quote "rax")))
             (InstrCons (NegQ (RegX86 (quote "rax"))) InstrNil)
        NotC arg ->
          let arg' = selectInstrsArg arg
          in InstrCons (MovQ arg' (RegX86 (quote "rax")))
             (InstrCons (XorQ (IntX86 1) (RegX86 (quote "rax"))) InstrNil)
        PrimC p a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_prim p
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case p of
               AddP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rax")))
                     a2' = selectInstrsArg a2
                     instr2 = (AddQ a2' (RegX86 (quote "rax")))
                 in InstrCons instr1 (InstrCons instr2 InstrNil)
               SubP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rax")))
                     a2' = selectInstrsArg a2
                     instr2 = (SubQ a2' (RegX86 (quote "rax")))
                 in InstrCons instr1 (InstrCons instr2 InstrNil)
        CmpC c a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_cmp c
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case c of
               EqP ->
                 let a1' = selectInstrsArg a1
                     a2' = selectInstrsArg a2
                     instr1 = (CmpQ a1' a2')
                     instr2 = (SetEQ (quote "al"))
                     instr3 = (MovzbQ (RegX86 (quote "al")) (RegX86 (quote "rax")))
                 in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 InstrNil))
               -- LtP -> _todo

    SeqC stm rst ->
      -- TRAVERSAL: stm is traversed (random access)
      case stm of
        AssignC v exp ->
          case exp of
            ArgC arg ->
              let arg' = selectInstrsArg arg
                  instr1 = (MovQ arg' (VarX86 v))
                  instrs' = selectInstrsTail rst
              in InstrCons instr1 instrs'
            NegC arg ->
              let arg' = selectInstrsArg arg
                  instr1 = (MovQ arg' (VarX86 v))
                  instr2 = (NegQ (VarX86 v))
                  instrs' = selectInstrsTail rst
              in InstrCons instr1 (InstrCons instr2 instrs')
            NotC arg ->
              let arg' = selectInstrsArg arg
                  instr1 = (MovQ arg' (VarX86 v))
                  instr2 = (XorQ (IntX86 1) (VarX86 v))
                  instrs' = selectInstrsTail rst
              in InstrCons instr1 (InstrCons instr2 instrs')
            PrimC p a1 a2 ->
              let -- TRAVERSAL: smallish traversal. OK.
                  _ = trav_prim p
                  _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
              in case p of
                   AddP ->
                     let a1' = selectInstrsArg a1
                         instr1 = (MovQ a1' (VarX86 v))
                         a2' = selectInstrsArg a2
                         instr2 = (AddQ a2' (VarX86 v))
                         instrs' = selectInstrsTail rst
                     in InstrCons instr1 (InstrCons instr2 instrs')
                   SubP ->
                     let a1' = selectInstrsArg a1
                         instr1 = (MovQ a1' (VarX86 v))
                         a2' = selectInstrsArg a2
                         instr2 = (SubQ a2' (VarX86 v))
                         instrs' = selectInstrsTail rst
                     in InstrCons instr1 (InstrCons instr2 instrs')
            CmpC c a1 a2 ->
              let -- TRAVERSAL: smallish traversal. OK.
                  _ = trav_cmp c
                  _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
              in case c of
                   EqP ->
                     let a1' = selectInstrsArg a1
                         a2' = selectInstrsArg a2
                         instr1 = (CmpQ a1' a2')
                         instr2 = (SetEQ (quote "al"))
                         instr3 = (MovzbQ (RegX86 (quote "al")) (VarX86 v))
                         instrs' = selectInstrsTail rst
                     in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 instrs'))
                   -- LtP -> _todo

    IfC thn els tst ->
      case tst of
        ArgC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (CmpQ arg' (IntX86 1))
              instr2 = (JumpEQ thn)
              instr3 = (JumpQ els)
          in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 InstrNil))
        NegC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (MovQ arg' (RegX86 (quote "rbx")))
              instr2 = (NegQ (RegX86 (quote "rbx")))
              instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
              instr4 = (JumpEQ thn)
              instr5 = (JumpQ els)
          in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 InstrNil))))
        NotC arg ->
          let arg' = selectInstrsArg arg
              instr1 = (MovQ arg' (RegX86 (quote "rbx")))
              instr2 = (XorQ (IntX86 1) (RegX86 (quote "rbx")))
              instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
              instr4 = (JumpEQ thn)
              instr5 = (JumpQ els)
          in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 InstrNil))))
        PrimC p a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_prim p
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case p of
               AddP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rbx")))
                     a2' = selectInstrsArg a2
                     instr2 = (AddQ a2' (RegX86 (quote "rbx")))
                     instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
                     instr4 = (JumpEQ thn)
                     instr5 = (JumpQ els)
                 in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 InstrNil))))
               SubP ->
                 let a1' = selectInstrsArg a1
                     instr1 = (MovQ a1' (RegX86 (quote "rbx")))
                     a2' = selectInstrsArg a2
                     instr2 = (SubQ a2' (RegX86 (quote "rbx")))
                     instr3 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
                     instr4 = (JumpEQ thn)
                     instr5 = (JumpQ els)
                     in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 InstrNil))))
        CmpC c a1 a2 ->
          let -- TRAVERSAL: smallish traversal. OK.
              _ = trav_cmp c
              _ = trav_arg a1 -- shouldn't be necessary since selectInstrsArg traverses a1 before we access a2.
          in case c of
               EqP ->
                 let
                     a1' = selectInstrsArg a1
                     a2' = selectInstrsArg a2
                     instr1 = (CmpQ a1' a2')
                     instr2 = (SetEQ (quote "al"))
                     instr3 = (MovzbQ (RegX86 (quote "al")) (RegX86 (quote "rbx")))
                     instr4 = (CmpQ (RegX86 (quote "rbx")) (IntX86 1))
                     instr5 = (JumpEQ thn)
                     instr6 = (JumpQ els)
                 in InstrCons instr1 (InstrCons instr2 (InstrCons instr3 (InstrCons instr4 (InstrCons instr5 (InstrCons instr6 InstrNil)))))


selectInstrsArg :: Arg -> ArgX86
selectInstrsArg arg =
  case arg of
    IntArg i -> IntX86 i
    TrueArg  -> IntX86 1
    FalseArg -> IntX86 0
    VarArg v -> VarX86 v

--------------------------------------------------------------------------------

makeHomes :: List Sym -> HomesEnv
makeHomes ls =
  let em :: HomesEnv
      em = empty_int_env
  in ifoldl_ll (\acc i v ->
                  let stack_loc = 0 - (8 + (8 * i))
                  in insert_int_env acc v stack_loc)
     em
     ls

assignHomes :: PseudoX86 -> PseudoX86
assignHomes prg =
  case prg of
    ProgramX86 ty locals instrs ->
      let homes = makeHomes locals
          -- homes :: HomesEnv
          -- homes = empty_int_env
          em :: List Sym
          em = alloc_ll
      in ProgramX86 ty em (assignHomesInstrs homes instrs)

assignHomes_par :: PseudoX86 -> PseudoX86
assignHomes_par prg =
  case prg of
    ProgramX86 ty locals instrs ->
      let homes = makeHomes locals
          -- homes :: HomesEnv
          -- homes = empty_int_env
          em :: List Sym
          em = alloc_ll
      in ProgramX86 ty em (assignHomesInstrs_par homes instrs)

assignHomesInstrs :: HomesEnv -> Instrs -> Instrs
assignHomesInstrs homes instrs =
  case instrs of
    InstrCons instr rst ->
      let instr' = assignHomesInstr homes instr
          rst' = assignHomesInstrs homes rst
      in InstrCons instr' rst'
    InstrNil -> InstrNil
    InstrAppend instr1 instr2 ->
      let instr1' = assignHomesInstrs homes instr1
          instr2' = assignHomesInstrs homes instr2
      in InstrAppend instr1' instr2'

assignHomesInstrs_par :: HomesEnv -> Instrs -> Instrs
assignHomesInstrs_par homes instrs =
  case instrs of
    InstrCons instr rst ->
      let instr' = assignHomesInstr homes instr
          rst' = assignHomesInstrs homes rst
      in InstrCons instr' rst'
    InstrNil -> InstrNil
    InstrAppend instr1 instr2 ->
      let instr1' = spawn (assignHomesInstrs_par homes instr1)
          instr2' = assignHomesInstrs_par homes instr2
          _ = sync
      in InstrAppend instr1' instr2'

assignHomesInstr :: HomesEnv -> Instr -> Instr
assignHomesInstr homes instr =
  case instr of
    AddQ a1 a2 -> AddQ (assignHomesArgX86 homes a1) (assignHomesArgX86 homes a2)
    SubQ a1 a2 -> SubQ (assignHomesArgX86 homes a1) (assignHomesArgX86 homes a2)
    NegQ a1 -> NegQ (assignHomesArgX86 homes a1)
    XorQ a1 a2 -> XorQ (assignHomesArgX86 homes a1) (assignHomesArgX86 homes a2)
    SetEQ r -> SetEQ r
    CmpQ a1 a2 -> CmpQ (assignHomesArgX86 homes a1) (assignHomesArgX86 homes a2)
    MovQ a1 a2 -> MovQ (assignHomesArgX86 homes a1) (assignHomesArgX86 homes a2)
    MovzbQ a1 a2 -> MovzbQ (assignHomesArgX86 homes a1) (assignHomesArgX86 homes a2)
    JumpQ lbl -> JumpQ lbl
    JumpEQ lbl -> JumpEQ lbl
    PushQ a1 -> PushQ (assignHomesArgX86 homes a1)
    PopQ a1 -> PopQ (assignHomesArgX86 homes a1)
    RetQ -> RetQ


assignHomesArgX86 :: HomesEnv -> ArgX86 -> ArgX86
assignHomesArgX86 homes arg =
  case arg of
    IntX86 i -> IntX86 i
    VarX86 v -> DerefX86 (quote "rbp") (lookup_int_env homes v)
    RegX86 r -> RegX86 r
    DerefX86 r o -> DerefX86 r o

--------------------------------------------------------------------------------

compile0 :: A -> A
compile0 p0 =
  let p1 = typecheckA p0
      p2 = uniqifyA p1
  in p2

compile0_par :: A -> A
compile0_par p0 =
  let p1 = typecheckA_par p0
      p2 = uniqifyA_par p1
  in p2

compile1 :: A -> C
compile1 p0 =
  let p1 = typecheckA p0
      p2 = uniqifyA p1
      p3 = explicateControl p2
  in p3

compile1_par :: A -> C
compile1_par p0 =
  let p1 = typecheckA_par p0
      p2 = uniqifyA_par p1
      p3 = explicateControl_par p2
  in p3

compile2 :: A -> PseudoX86
compile2 p0 =
  let p1 = typecheckA p0
      p2 = uniqifyA p1
      p3 = explicateControl p2
      -- p4 = optimizeJumps p3
      p5 = selectInstrs p3
      p6 = assignHomes p5
  in p6

compile2_par :: A -> PseudoX86
compile2_par p0 =
  let p1 = typecheckA_par p0
      p2 = uniqifyA_par p1
      p3 = explicateControl_par p2
      -- p4 = optimizeJumps p3
      p5 = selectInstrs_par p3
      p6 = assignHomes_par p5
  in p6

-- compile3 :: A -> PseudoX86
-- compile3 p0 =
--   let p1 = typecheckA p0
--       _ = print_program_a p1
--       _ = print_newline()
--       _ = print_newline()
--       p2 = uniqifyA p1
--       _ = print_program_a p2
--       _ = print_newline()
--       _ = print_newline()
--       p3 = explicateControl p2
--       _ = print_program_c p3
--       _ = print_newline()
--       _ = print_newline()
--       -- p4 = optimizeJumps p3
--       -- _ = print_program_c p4
--       -- _ = print_newline()
--       p5 = selectInstrs p3
--       _ = print_pseudox86 p5
--       p6 = assignHomes p5
--       _ = print_pseudox86 p6
--   in p6

--------------------------------------------------------------------------------

make_big_ex2 :: Int -> ExpA
make_big_ex2 n =
  if n <= 0
  then SimplA (ArgA (IntArg 1))
  else
    -- let v2 = gensym
    let v2 = quote "v2"
    in (LetA2 v2 (ArgA (IntArg (n-1))) (make_big_ex2 (n-1)))

make_big_ex :: Int -> Int -> ExpA
make_big_ex n d =
  -- SMALL
  if d > 6
  -- -- OTHERWISE
  -- if d > 10
  then make_big_ex2 n
  else
    -- let v1 = gensym
    let v1 = quote "v1"
    in LetA v1 (ArgA (IntArg n))
       (IfA (CmpA EqP (VarArg v1) (IntArg 0))
         (make_big_ex n (d+1))
         (make_big_ex n (d+1)))
{-

small_ex :: A
small_ex = ProgramA intTy
          (LetA (quote "v0") (ArgA (IntArg 20))
           (LetA (quote "v1") (ArgA (IntArg 22))
            (LetA (quote "res") (PrimA AddP (VarArg (quote "v0")) (VarArg (quote "v1")))
             (IfA
              (CmpA EqP (VarArg (quote "v0")) (VarArg (quote "v1")))
              (SimplA (ArgA (VarArg (quote "v0"))))
              (SimplA (ArgA (VarArg (quote "v1"))))))))

-}

gibbon_main =
  let ex = make_big_ex sizeParam 0
      {- _ = print_expa ex -}
      p = ProgramA intTy ex
      compiled = timeit (compile2 p)
      -- _ = print_pseudox86 compiled
      -- _ = printsym (quote "\n")
      compiled_par = timeit (compile2_par p)
      -- _ = print_pseudox86 compiled_par
  in ()

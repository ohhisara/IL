import Syntax
import Instructions

type Var = String
type Addr = String --ou int?
type Stack = [ValueSECD]
type EnvSECD = [ValueSECD]
type Dump = [(Stack,EnvSECD,Code)]
type Symtable = [Var]
type Closure = (Code, EnvSECD)
type Memory = Addr -> Closure
type SECD = (Stack,EnvSECD,Code,Dump,Memory)

data ValueSECD = Prim Int 
 | Address Addr

compile:: Term -> Symtable -> [Instr]
compile (Const c) sym = [LDC c]
compile (Var i) sym = [LD v]
 where v = elemIndex i sym
compile (Lambda i t) sym= [LDF ((compile t sym)++[RTN])]
compile (App t1 t2) sym = (compile t1 sym) ++ (compile t2 sym) ++ [AP]
compile (t1 :+ t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [ADD]
compile (t1 :- t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [SUB]
compile (t1 :/ t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [MUL]
compile (t1 :* t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [DIV]
compile (Ifzero t1 t2 t3) sym= (compile t1 sym) ++ [SEL c1 c2]
 where c1 =
compile (Let i t1 t2) sym= compile (App (Lambda i t2) t1) sym

execute::SECD -> SECD
execute (s,e,(LDC i):c,d,m) = (((Prim i):s),e,c,d,m)
execute (s,e, (LD i):c,d,m) = (v:s,e,c,d,m)
 where v = lookupId s e 
execute (s,e,(LDF code):c,d,m) = (a:s,e,c,d,)


lookupId::Id->Env-> Maybe ValueSECD
lookupId id ((x,v):xs) 
 | id == x = Just v
 | otherwise = lookupId id xs

next::Memory-> 
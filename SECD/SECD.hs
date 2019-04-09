import Syntax
import Instructions
--import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Parse

type Addr = Int
type Stack = [ValueSECD]
type EnvSECD = [ValueSECD]
type Dump = [(Stack,EnvSECD,Code)]
type Symtable = [Id]
type Closure = (Code, EnvSECD)
type Memory = Map Addr Closure
type SECD = (Stack,EnvSECD,Code,Dump,Memory)

data ValueSECD = Prim Int 
 | Address Addr
 deriving(Show)

compiler::String -> Code
compiler s = compile (Parse.parse s) []

compile:: Term -> Symtable -> Code
compile (Const c) sym = [LDC c]
compile (Var i) sym = [LD v]
 where v = elemIndex i sym
compile (Lambda i t) sym= [LDF ((compile t sym')++[RTN])]
	where sym' = i:sym
compile (App t1 t2) sym = (compile t1 sym) ++ (compile t2 sym) ++ [AP]
compile (t1 :+ t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [ADD]
compile (t1 :- t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [SUB]
compile (Ifzero t1 t2 t3) sym= (compile t1 sym) ++ [SEL c1 c2]
	where 
		c1 = ((compile t2 sym) ++ [JOIN])
		c2 = ((compile t3 sym) ++ [JOIN])
compile (Let i t1 t2) sym= compile (App (Lambda i t2) t1) sym
compile (Fix (Lambda f (Lambda x e))) sym = [LDRF (compile e sym' ++ [RTN])]
	where sym' = x:f:sym

--call by name
{-execute::SECD -> SECD
execute (s,e,(LDC i):c,d,m) = (((Prim i):s),e,c,d,m)
execute (s,e, (LD i):c,d,m) = (v:s,e,c,d,m)
 where v = lookupId i e 
execute (s,e,(LDF code):c,d,m) = 
 let a = next m
     store' = Map.insert a (code,e) m
 in ((Address a):s,e,c,d,store')
execute (s,e,(LDRF c'):c,d,m) = ((Address a):s,e,c,d,m')
 where a = next m
       m' =(Map.insert a (c',((Address a):e)) m)
execute ((Prim v1):(Prim v2):s,e, (ADD):c,d,m) = ((Prim (v1+v2)):s,e,c,d,m)
execute ((Prim v1):(Prim v2):s,e, (SUB):c,d,m) = ((Prim (v1-v2)):s,e,c,d,m)
execute ((Prim v1):(Prim v2):s,e, (MUL):c,d,m) = ((Prim (v1*v2)):s,e,c,d,m)
execute (v:(Address a):s,e,AP:c,d,m) = (v:[(Address a)],v:e',[AP]++c',(s,e,c):d,m)
 where (c',e')= 
 	case (Map.lookup b m) of
 		Just (c',e') -> (c',e')
 		Nothing -> error "hm"
execute (v:(Address a):s,e,AP:c,d,m) = ([],v:e',c',(s,e,c):d,m)
 where (c',e')= 
 	case (Map.lookup a m) of
 		Just (c',e') -> (c',e')
 		Nothing -> error "hm"
execute (v:s,e,RTN:c,(s',e',c'):d,m) = (v:s',e',c',d,m)
execute ((Prim 0):s,e,(SEL c1 c2):c,d,m) = (s,e,c1,([],[],c):d,m)
execute ((Prim _):s,e,(SEL c1 c2):c,d,m) = (s,e,c2,([],[],c):d,m)
execute (s,e,JOIN:c,(_,_,c'):d,m) = (s,e,c',d,m)
execute (s,e,[HALT],d,m) = (s,e,[],d,m)
execute (s,e,[],d,m) = (s,e,[],d,m)-}

--call by value
execute::SECD -> SECD
execute (s,e,(LDC i):c,d,m) = (((Prim i):s),e,c,d,m)
execute (s,e, (LD i):c,d,m) = (v:s,e,c,d,m)
 where v = lookupId i e 
execute (s,e,(LDF code):c,d,m) = 
 let a = next m
     store' = Map.insert a (code,e) m
 in ((Address a):s,e,c,d,store')
execute (s,e,(LDRF c'):c,d,m) = ((Address a):s,e,c,d,m')
 where a = next m
       m' =(Map.insert a (c',((Address a):e)) m)
execute ((Prim v1):(Prim v2):s,e, (ADD):c,d,m) = ((Prim (v1+v2)):s,e,c,d,m)
execute ((Prim v1):(Prim v2):s,e, (SUB):c,d,m) = ((Prim (v1-v2)):s,e,c,d,m)
execute ((Prim v1):(Prim v2):s,e, (MUL):c,d,m) = ((Prim (v1*v2)):s,e,c,d,m)
execute (v:(Address a):s,e,AP:c,d,m) = ([],v:e',c',(s,e,c):d,m)
 where (c',e')= 
 	case (Map.lookup a m) of
 		Just (c',e') -> (c',e')
 		Nothing -> error "hm"
execute (v:s,e,RTN:c,(s',e',c'):d,m) = (v:s',e',c',d,m)
execute ((Prim 0):s,e,(SEL c1 c2):c,d,m) = (s,e,c1,([],[],c):d,m)
execute ((Prim _):s,e,(SEL c1 c2):c,d,m) = (s,e,c2,([],[],c):d,m)
execute (s,e,JOIN:c,(_,_,c'):d,m) = (s,e,c',d,m)
execute (s,e,[HALT],d,m) = (s,e,[],d,m)
execute (s,e,[],d,m) = (s,e,[],d,m)

elemIndex::String -> Symtable -> Int
elemIndex s [] = 0
elemIndex s (x:xs) 
 | s == x = 0 
 | otherwise = 1 + (elemIndex s xs)

lookupId::Int->EnvSECD-> ValueSECD
lookupId i env = env!!i

next::Memory -> Addr
next store = 1 + (Map.size store)

run::Code -> IO()
run code = sequence_ [print state | state <- trace]
 where states = iterate execute ([],[],code++[HALT],[],Map.empty)
       trace = takeWhile (not.final) states
       final (s,e,c,d,m) = null c
import Syntax
import Instructions
--import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Parse

type Addr = Int
type Stack = [Elem]
data Elem = V ValueSECD
 | C Code
 deriving (Show)
type EnvSECD = [Elem]
type Dump = [(Stack,EnvSECD,Code)]
type Symtable = [Id]
type Closure = (Code, EnvSECD)
type Memory = Map Addr Closure
type SECD = (Stack,EnvSECD,Code,Dump,Memory,DelayStack)
type DelayStack = [Code]

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
compile (App t1 t2) sym = (compile t1 sym) ++ [DELAY (compile t2 sym)] ++ [AP]
compile (t1 :+ t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [ADD]
compile (t1 :- t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [SUB]
compile (Ifzero t1 t2 t3) sym= (compile t1 sym) ++ [SEL c1 c2]
	where 
		c1 = ((compile t2 sym) ++ [JOIN])
		c2 = ((compile t3 sym) ++ [JOIN])
compile (Let i t1 t2) sym= compile (App (Lambda i t2) t1) sym
compile (Fix (Lambda f (Lambda x e))) sym = [LDRF (compile e sym' ++ [RTN])]
	where sym' = x:f:sym

execute::SECD -> SECD
execute (s,e,(LDC i):c,d,m,ds) = ((V (Prim i):s),e,c,d,m,ds)
execute (s,e, (LD i):c,d,m,ds) --procurar indice no ambiente
 | (isVal v) = (V (getVal v):s,e,c,d,m,ds) -- se for value guardar na stack
 | otherwise = (s,e,(getCode v)++c,d,m,ds) -- se for codigo acrescentar antes do codigo a avaliar
 where v = lookupId i e 
execute (s,e,(LDF code):c,d,m,ds) = 
 let a = next m
     store' = Map.insert a (code,e) m
 in (V (Address a):s,e,c,d,store',ds)
execute (s,e,(LDRF c'):c,d,m,ds) = (V (Address a):s,e,c,d,m',ds)
 where a = next m
       m' =(Map.insert a (c',(V (Address a):e)) m)
execute (V (Prim v1):V (Prim v2):s,e, (ADD):c,d,m,ds) = (V (Prim (v1+v2)):s,e,c,d,m,ds)
execute (V (Prim v1):V (Prim v2):s,e, (SUB):c,d,m,ds) = (V (Prim (v1-v2)):s,e,c,d,m,ds)
execute (V (Prim v1):V (Prim v2):s,e, (MUL):c,d,m,ds) = (V (Prim (v1*v2)):s,e,c,d,m,ds)
execute (v:(V (Address a)):s,e,AP:c,d,m,ds) = ([],v:e',c',(s,e,c):d,m,ds)
 where (c',e')= 
 	case (Map.lookup a m) of
 		Just (c',e') -> (c',e')
 		Nothing -> error "hm"
execute (v:s,e,RTN:c,(s',e',c'):d,m,ds) = (v:s',e',c',d,m,ds)
execute (V (Prim 0):s,e,(SEL c1 c2):c,d,m,ds) = (s,e,c1,([],[],c):d,m,ds)
execute (V (Prim _):s,e,(SEL c1 c2):c,d,m,ds) = (s,e,c2,([],[],c):d,m,ds)
execute (s,e,JOIN:c,(_,_,c'):d,m,ds) = (s,e,c',d,m,ds)
execute (s,e,(DELAY c'):c,d,m,ds) = ((C c'):s,e,c,d,m,ds) --quando encontra argumento guarda na stack sem avaliar
execute (s,e,[HALT],d,m,ds) = (s,e,[],d,m,ds)
execute (s,e,[],d,m,ds) = (s,e,[],d,m,ds)

isVal::Elem -> Bool
isVal (V _) = True
isVal (C _) = False

getVal::Elem -> ValueSECD
getVal (V v) = v
getVal _ = error "val"

getCode::Elem -> Code
getCode (C c) = c 
getCode _ = error "code"

elemIndex::String -> Symtable -> Int
elemIndex s [] = 0
elemIndex s (x:xs) 
 | s == x = 0 
 | otherwise = 1 + (elemIndex s xs)

lookupId::Int->EnvSECD-> Elem
lookupId i env = env!!i

next::Memory -> Addr
next store = 1 + (Map.size store)

run::Code -> IO()
run code = sequence_ [print state | state <- trace]
 where states = iterate execute ([],[],code++[HALT],[],Map.empty,[])
       trace = takeWhile (not.final) states
       final (s,e,c,d,m,ds) = null c
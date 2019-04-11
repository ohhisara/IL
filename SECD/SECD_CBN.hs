import Syntax
import Instructions
--import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Parse

type Addr = Int
type Stack = [Elem] -- stack pode aceitar codigo (codigo por avaliar)
data Elem = V ValueSECD -- elemento para stack e env  
 | C Code
 deriving (Show)
type EnvSECD = [Elem] -- amb
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
compile (App t1 (App a b)) sym = (compile t1 sym) ++ [DELAY (compile (App a b) sym)] ++ [AP]
compile (App t1 t2) sym = (compile t1 sym) ++ (compile t2 sym) ++ [AP]
compile (t1 :+ t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [ADD]
compile (t1 :- t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [SUB]
compile (t1 :* t2) sym= (compile t1 sym) ++ (compile t2 sym) ++ [MUL]
compile (Ifzero t1 t2 t3) sym= (compile t1 sym) ++ [SEL c1 c2]
	where 
		c1 = ((compile t2 sym) ++ [JOIN])
		c2 = ((compile t3 sym) ++ [JOIN])
compile (Let i t1 t2) sym= compile (App (Lambda i t2) t1) sym
compile (Fix (Lambda f (Lambda x e))) sym = [LDRF (compile e sym' ++ [RTN])]
	where sym' = x:f:sym

execute::SECD -> SECD
execute (s,e,(LDC i):c,d,m) = ((V (Prim i):s),e,c,d,m)
execute (s,e, (LD i):c,d,m) --procurar indice no ambiente
 | (isVal v) = (v:s,e,c,d,m) -- se for value guardar na stack
 | otherwise = (s,e,(getCode v)++c,d,m) -- se for codigo acrescentar antes do codigo a avaliar
 where v = lookupId i e 
execute (s,e,(LDF code):c,d,m) = 
 let a = next m
     store' = Map.insert a (code,e) m
 in (V (Address a):s,e,c,d,store')
execute (s,e,(LDRF c'):c,d,m) = (V (Address a):s,e,c,d,m')
 where a = next m
       m' =(Map.insert a (c',(V (Address a):e)) m)
execute (V (Prim v1):V (Prim v2):s,e, (ADD):c,d,m) = (V (Prim (v1+v2)):s,e,c,d,m)
execute (V (Prim v1):V (Prim v2):s,e, (SUB):c,d,m) = (V (Prim (v2-v1)):s,e,c,d,m)
execute (V (Prim v1):V (Prim v2):s,e, (MUL):c,d,m) = (V (Prim (v1*v2)):s,e,c,d,m)
execute (v:(V (Address a)):s,e,AP:c,d,m) = ([],v:e',c',(s,e,c):d,m)
 where (c',e')= 
 	case (Map.lookup a m) of
 		Just (c',e') -> (c',e')
 		Nothing -> error "hm"
execute (v:s,e,RTN:c,(s',e',c'):d,m) = (v:s',e',c',d,m)
execute (V (Prim 0):s,e,(SEL c1 c2):c,d,m) = (s,e,c1,([],[],c):d,m)
execute (V (Prim _):s,e,(SEL c1 c2):c,d,m) = (s,e,c2,([],[],c):d,m)
execute (s,e,JOIN:c,(_,_,c'):d,m) = (s,e,c',d,m)
execute (s,e,(DELAY c'):c,d,m) = ((C c'):s,e,c,d,m) --quando encontra argumento guarda na stack sem avaliar
execute (s,e,[HALT],d,m) = (s,e,[],d,m)
execute (s,e,[],d,m) = (s,e,[],d,m)

isVal::Elem -> Bool
isVal (V _) = True
isVal (C _) = False

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
 where states = iterate execute ([],[],code++[HALT],[],Map.empty)
       trace = takeWhile (not.final) states
       final (s,e,c,d,m) = null c

--exemplos
ex1= (compiler "(\\x.x)(2)")
ex2= [LDC 1, LDC 2, LDC 3, MUL, ADD]
ex3= [LDF [LD 0, LDC 1, ADD, RTN]]
ex4= (compiler "(\\x.ifzero x 1 x+1)(1)")
ex5= [LDF [LD 0, SEL [LDC 1, JOIN] [LDC 0, JOIN], RTN]]
ex6= (compiler "let x = 1 in x+2")
ex7= (compiler "(\\x.(\\y.ifzero x+y 1 x+y)(0))(2)")
--está mal (entra em ciclo)
ex8= (compile (Let "fact" (Fix (Lambda "f" (Lambda "n" (Ifzero (Var "n") (Const 1) ((Var "n"):*(App (Var "f") ((Var "n"):-(Const 1)))))))) (App (Var "fact") (Const 3))) [])
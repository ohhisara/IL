import Syntax

eval2::Term->Env->Value2
eval2 (Var i) env = case (lookupId i env) of
	Just v -> v
	Nothing -> error "Variável não existe"
eval2 (Const i) env = Cons i
eval2 (Lambda i t) env= Closure (Lambda i t) env
eval2 (App t1 t2) env= (apply (eval2 t1 env)$!(eval2 t2 env))
eval2 (Ifzero t1 t2 t3) env = case (eval2 t1 env) of
	(Cons 0) -> (eval2 t2 env)
	(Cons _) -> (eval2 t3 env)
eval2 (t1 :+ t2) env= Cons (primitive (+) (eval2 t1 env) (eval2 t2 env))
eval2 (t1 :- t2) env= Cons (primitive (-) (eval2 t1 env) (eval2 t2 env))
eval2 (t1 :* t2) env= Cons (primitive (*) (eval2 t1 env) (eval2 t2 env))
eval2 (t1 :/ t2) env= Cons (primitive (div) (eval2 t1 env) (eval2 t2 env))
eval2 (Let id t1 t2) env= eval2 (App (Lambda id t2) t1) env
eval2 (Fix (Lambda f e)) env = let v = Closure e ((f,v):env)
 in v 

lookupId::Id->Env-> Maybe Value2
lookupId id ((x,v):xs) 
 | id == x = Just v
 | otherwise = lookupId id xs

apply::Value2->Value2->Value2
apply (Closure (Lambda x e) env) v= eval2 e ((x,v):env)

primitive::(Int->Int->Int) -> Value2 -> Value2 -> Int
primitive (+) (Cons v1) (Cons v2) = v1 + v2
primitive (-) (Cons v1) (Cons v2) = v1 - v2
primitive (*) (Cons v1) (Cons v2) = v1 * v2
primitive (div) (Cons v1) (Cons v2) = v1 `div` v2

ppValue::Value2 -> String
ppValue (Cons n) = show n
ppValue (Closure (Lambda id e)  env )= "( \\" ++ id ++ "." ++ ppTerm e ++ "), " ++ ppEnv env

ppEnv::Env -> String
ppEnv ((x,v):xs) = "[" ++ x ++ " -> " ++ ppValue v ++ ", " ++ ppEnv xs

ppTerm::Term -> String
ppTerm (Const n) = show n
ppTerm (Var v) = v
ppTerm (Lambda id e) = "\\" ++ id ++ "." ++ ppTerm e
ppTerm (App t1 t2) = "[" ++ ppTerm t1 ++ "]" ++ "[" ++ ppTerm t2 ++ "]"
ppTerm (Ifzero t1 t2 t3) = "if (" ++ ppTerm t1 ++ " == 0): " ++ ppTerm t2 ++ " else: " ++ ppTerm t3 
ppTerm (Let id t1 t2) = "let " ++ id ++ " = " ++ ppTerm t1 ++ " in " ++ ppTerm t2
ppTerm (Fix t1 ) = "fix (" ++ ppTerm t1 ++ ")"
ppTerm (t1:+t2) = (ppTerm t1) ++ " + " ++ (ppTerm t2)
ppTerm (t1:-t2) = (ppTerm t1) ++ " - " ++ (ppTerm t2)
ppTerm (t1:*t2) = (ppTerm t1)++ " * " ++(ppTerm t2)
ppTerm (t1:/t2) = (ppTerm t1)++ " / " ++(ppTerm t2)


out::Term -> String
out t = ppValue (eval2 t [])
import Syntax

eval2::Term->Env->Value2
eval2 (Var i) env = case (lookup (Var i) env) of
	Just v -> v
	Nothing -> error "Variável não existe"
eval2 (Const i) env = Cons i
eval2 (Lambda i t) env= Closure ((Lambda i t),env)
eval2 (App t1 t2) env= (apply (eval t1 env)$!(eval t2 env))
eval2 (Ifzero t1 t2 t3) env = case (eval2 t1 env) of
	(Cons 0) -> (eval2 t2 env)
	(Cons _) -> (eval2 t3 env)
eval2 (t1 :+ t2) env= Cons (primitive (+) (eval2 t1 env) (eval2 t2 env))
eval2 (t1 :- t2) env= Cons (primitive (-) (eval2 t1 env) (eval2 t2 env))
eval2 (t1 :* t2) env= Cons (primitive (*) (eval2 t1 env) (eval2 t2 env))
eval2 (t1 :/ t2) env= Cons (primitive (div) (eval2 t1 env) (eval2 t2 env))
eval2 (Let id t1 t2) env= eval2 (App (Lambda id t2) t1) env
eval2 (Fix (Lambda f e)) env =
 let v = e 
 env' = (f,e):env
 	in eval2 v env'

lookup::Id-> Env-> Maybe Value2
lookup id (x,v)
 | id == x = v
 | otherwise = Nothing

apply::Value->Value->Value
apply (Closure (Lambda x e) env) = eval2 (x,v):env

primitive::(Int->Int->Int) -> Value -> Value -> Int
primitive (+) (Const v1) (Const v2) = v1 + v2
primitive (-) (Const v1) (Const v2) = v1 - v2
primitive (*) (Const v1) (Const v2) = v1 * v2
primitive (div) (Const v1) (Const v2) = v1 `div` v2
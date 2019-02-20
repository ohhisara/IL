import Syntax

--eval(App (Fix (Lambda "f" (Lambda "x" (Ifzero (Var "x") (Const 1) ((Var "x"):*(App (Var "f") ((Var "x"):-(Const 1)))))))) (Const 3))

eval:: Term -> Value
eval (Var i) = Var i
eval (Const i) = Const i
eval (Lambda i t) = Lambda i t
eval (App t1 t2) = (apply (eval t1)$!(eval t2))
eval (Ifzero t1 t2 t3)= case (eval t1) of
	(Const 0) -> (eval t2)
	(Const _) -> (eval t3)
eval (t1 :+ t2)= Const (primitive (+) (eval t1) (eval t2))
eval (t1 :- t2)= Const (primitive (-) (eval t1) (eval t2))
eval (t1 :* t2)= Const (primitive (*) (eval t1) (eval t2))
eval (t1 :/ t2)= Const (primitive (div) (eval t1) (eval t2))
eval (Let id t1 t2) = eval (App (Lambda id t2) t1)
eval (Fix t) = fix t

fix::Term->Value
fix (Lambda f (Lambda x t)) = Lambda x (subst t f (Fix (Lambda f (Lambda x t))))

primitive::(Int->Int->Int) -> Value -> Value -> Int
primitive (+) (Const v1) (Const v2) = v1 + v2
primitive (-) (Const v1) (Const v2) = v1 - v2
primitive (*) (Const v1) (Const v2) = v1 * v2
primitive (div) (Const v1) (Const v2) = v1 `div` v2

apply::Value->Value->Value
apply (Lambda i t) v = eval (subst t i v)
apply _ _ = error "hm"

subst::Term->Id->Value->Term
subst (Const c) id v= Const c
subst (Var i) id (Var c)
 | i == id = (Var c)
 | otherwise = (Var i)
subst (Var i) id (Const c)
 | i == id = (Const c)
 | otherwise = (Var i)
subst (Var i) id (Lambda idd t) 
 | i == id = (Lambda idd t)
 | otherwise = (Var i)
subst (Var i) id (Fix t)
 | i==id = (Fix t)
 |otherwise = (Var i)
subst (Lambda i t) id v = Lambda i (subst t id v)
subst (App t1 t2) id v = App (subst t1 id v) (subst t2 id v)
subst (Ifzero t1 t2 t3) id v = Ifzero (subst t1 id v) (subst t2 id v) (subst t3 id v)
subst (Let id t1 t2) idd v = Let id (subst t1 idd v) (subst t2 idd v) 
subst (Fix t) id v = Fix (subst t id v)
subst (t1:+t2) id v = (subst t1 id v):+(subst t2 id v)
subst (t1:-t2) id v = (subst t1 id v):-(subst t2 id v)
subst (t1:*t2) id v = (subst t1 id v):*(subst t2 id v)
subst (t1:/t2) id v = (subst t1 id v):/(subst t2 id v)

pp::Value -> String
pp (Const n) = show n
pp (Var v) = v
pp (Lambda id e) = "\\" ++ id ++ "." ++ ppTerm e

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
out t = pp (eval t)
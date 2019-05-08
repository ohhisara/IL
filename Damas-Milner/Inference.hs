module Inference where
    import Syntax
    import Substitution
    import Unification

    --inference algorithm
    infer:: Env -> Expr -> (Substitution, Type)
    infer env (Vari v) = lookupEnv v env
    infer env (Abs x expr) = (s1,t)
       where (s1,t1) = infer ((x,TP (Var "beta")):env) expr
             t = Function (applySubTypeList s1 (Var "beta")) t1
    infer env (App expr1 expr2) = (s,t)
     where (s1, t1) = infer env expr1
           (s2,t2) = infer (applySubEnvList s1 env) expr2
           v = unify (applySubTypeList s2 t1) (Function t2 (Var "beta"))
           s = applySubSubList v (applySubSubList s2 s1)
           t = applySubTypeList v (Var "beta")
    --infer env (Let x expr1 expr2) = do
    --    (s1,t2) <- infer env expr1

    --lookup variable in an environment
    lookupEnv::Id -> Env -> (Substitution,Type)
    lookupEnv _ [] = error "chegou ao fim do ambiente"
    lookupEnv x ((id,TP t):xs)
     | x == id = ([], t)
     | otherwise = lookupEnv x xs 
    lookupEnv x ((id,ForAll l t):xs)
     | x == id = ([], applySubTypeList (genSub l) t)
     | otherwise = lookupEnv x xs 

    --generate new vars
    genSub::[TVar] -> Substitution
    genSub [] = []
    genSub (v:vs) = (Var (v++"'"),v):(genSub vs)


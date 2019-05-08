module Substitution where
    import Syntax 

    --apply a Substitution to a Substitution
    applySubSub::(Type,TVar) -> Substitution -> Substitution
    applySubSub _ [] = []
    applySubSub (t,v) ((t1,v1):xs)
     | v == v1 = ((t,v1):(applySubSub (t,v) xs))
     | otherwise = applySubSub (t,v) xs
    
    applySubSubList::Substitution -> Substitution -> Substitution
    applySubSubList s [] = s
    applySubSubList [] s = s
    applySubSubList ((t,v):s1) s2 = (applySubSub (t,v) s2)++(applySubSubList s1 s2)

    --apply a substitution to a type
    applySubType:: (Type,TVar) -> Type -> Type
    applySubType (t,v) (Prim p) = (Prim p)
    applySubType (t,v) (Var v1)
     | v == v1 = t
     | otherwise = Var v1
    applySubType (t,v) (Function t1 t2)= 
        (Function (applySubType (t,v) t1 ) (applySubType (t,v) t2))
    
    applySubTypeList::Substitution -> Type -> Type
    applySubTypeList [] t = t
    applySubTypeList ((t1,v):s) t= applySubTypeList s (applySubType (t1,v) t)

    --apply substitution to an environment
    applySubEnvList::Substitution->Env->Env
    applySubEnvList [] env = env
    applySubEnvList ((x,t):xs) env = applySubEnvList xs (applySubEnv (x,t) env)

    applySubEnv::(Type,TVar)-> Env -> Env
    applySubEnv _ [] = []
    applySubEnv (t,v) ((id,ts):xs) 
     | v == id = (id,TP t):(applySubEnv (t,v) xs)
     |otherwise = (id,ts):(applySubEnv (t,v) xs)
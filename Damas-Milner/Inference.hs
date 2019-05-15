module Inference where
    import Syntax
    import Substitution
    import Unification
    import Debug.Trace
    import PrettyPrint

    --inference algorithm
    infer:: Env -> Expr -> (Substitution, Type)
    infer env (Const c) 
     | isBool c = ([],typeBool)
     | otherwise = ([],typeInt)

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
           
    infer env (Let x expr1 expr2) = (s,t2)
      where (s1,t1) =  infer env expr1
            (s2,t2) =
                  infer ( unionEnv (applySubEnvList s1 env) (applySubEnvList s1  [(x, (closure t1 env))])) expr2
            s = applySubSubList s2 s1

    --lookup variable in an environment
    lookupEnv::Id -> Env -> (Substitution,Type)
    lookupEnv _ [] = error "chegou ao fim do ambiente :("
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

    -- union of envs (?)
    unionEnv::Env->Env -> Env 
    unionEnv e1 e2 = e1 ++ e2 

    -- free variables in env (?)
    boundVarsEnv::Env -> [TVar]
    boundVarsEnv [] = []
    boundVarsEnv ((id,ForAll l t):xs) = l++(boundVarsEnv xs)
    boundVarsEnv ((id,TP t):xs) = boundVarsEnv xs
      
    --closure of a type with respect to env
    closure::Type -> Env -> TypeScheme
    closure t env = ForAll (notInList (freeVarsType t) (boundVarsEnv env)) t

    -- check what variables are not in given list of variables
    notIn::TVar -> [TVar] -> [TVar]
    notIn v (v1:vs1) 
     | v == v1 = [v]
     | otherwise = []++(notIn v vs1)

    notInList::[TVar] -> [TVar] -> [TVar]
    notInList l [] = []
    notInList [] l = l
    notInList (v:vs) l = (notIn v l)++(notInList vs l)

    isBool::String -> Bool 
    isBool "True" = True 
    isBool "False" = True 
    isBool _ = False  

    main::Env -> Expr-> String
    main env e = pp (infer env e)

    {-run::Env -> Expr -> IO()
    run env e = sequence_ [print state | state <- trace]
       where states = iterate (infer env e)
             trace = takeWhile (not.final) states
             final e = varExpr e 

    varExpr::Expr -> Bool
    varExpr (Vari _) = True
    varExpr _ = False-}



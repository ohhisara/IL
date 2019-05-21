module Inference where
    import Syntax
    import Substitution
    import Unification
    import Debug.Trace
    import PrettyPrint
    import Control.Monad.State

    --inference algorithm
    infer:: Env -> Expr -> Type -> (Substitution, Type)
    infer env (Const c) v
     | isBool c = ([],typeBool)
     | otherwise = ([],typeInt)

    infer env (Vari v) var = lookupEnv v env 

    infer env (Abs x expr) v = (s1,t)
       where newv = fresh v 
             (s1,t1) = infer ((x,TP newv):env) expr newv
             t = Function (applySubTypeList s1 newv) t1

    infer env (App expr1 expr2) var= (s,t)
     where newv = fresh var
           newv1 = fresh newv
           newv2 = fresh newv1
           (s1, t1) = infer env expr1 newv1
           (s2,t2) = infer (applySubEnvList s1 env) expr2 newv2
           v = 
            case unify (applySubTypeList s2 t1) (Function t2 newv) of 
                  Just v -> v 
                  Nothing -> error "hm"
           s = applySubSubList v (applySubSubList s2 s1)
           t = applySubTypeList v newv
           
    infer env (Let x expr1 expr2) v= (s,t2)
      where (s1,t1) =  infer env expr1 v
            (s2,t2) =
                  infer (unionEnv (applySubEnvList s1 env) (applySubEnvList s1  [(x, (closure t1 env))])) expr2 v
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
     
    --new var generation
    fresh:: Type -> Type 
    fresh (Var s) = Var (s++"'")

    type MyVar = Type
    nextVar::MyVar -> MyVar
    nextVar (Var v) = Var (v ++ "'")

    type MyVarMonad = State MyVar
    genNew :: MyVarMonad MyVar
    genNew = do {
                 x <- get;
                 put (nextVar x);
                 return x;
                 }
      --state (\st -> let st' = nextVar(st) in (st',st'))

    initVar::Type
    initVar = Var "Beta"
    
    {-try::MyVarMonad MyVar
    try = do
      evalState genNew initVar
      genNew-}




    {-genNewVar::MyVarMonad MyVar
    genNewVar = do
      s <- get
      put (getNext)
      return s
      case s of 
            (state t1 t2) -> return s
      n <- getNext
      case n of
            (state s1 s2 )-> put n-}
      
    --newVar = evalState genNewVar initVar
        
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


    --main::Env -> Expr-> IO()
    --main env e = pp (infer env e)

    {-run::Env -> Expr -> IO()
    run env e = sequence_ [print state | state <- trace]
       where states = iterate (infer env e)
             trace = takeWhile (not.final) states
             final e = varExpr e 

    varExpr::Expr -> Bool
    varExpr (Vari _) = True
    varExpr _ = False-}



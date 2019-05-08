module Unification where
    import Syntax
    import Substitution
    

    --unify list os types
    unifyAll::[Type] -> [Type] -> Substitution
    unifyAll (t:ts) (t1:ts1) = (unify t t1)++(unifyAll ts ts1)
    
    -- unify one pair of types
    unify::Type -> Type -> Substitution
    unify t1 t2
     | t1 == t2 = []
    unify (Var v) t = [(t,v)]
    unify t (Var v)= [(t,v)]
    unify (Function t1 t2) (Function t3 t4) = (applySubSubList s1 s2)
     where s1 = unify t1 t3
           s2 = unify (applySubTypeList s1 t2) (applySubTypeList s1 t4)

    -- check if var belongs to the type's free variables
    --isInFV::Type -> TVar -> Bool
    --isInFV t v 
    -- | v `elem` (freeVars t) = True
    -- | otherwise = False
    
    --freeVars::Type -> [TVar]
        
     



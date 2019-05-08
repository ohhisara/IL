module Unification where
    import Syntax

    

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



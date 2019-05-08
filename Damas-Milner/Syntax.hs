module Syntax where


    type Id = String
    type TVar= String

    data Expr = Vari Id
     | App Expr Expr
     | Abs Id Expr
     | Let Id Expr Expr
     deriving(Show)

    data TypeScheme = ForAll [TVar] Type
     | TP Type
    
    data Type = Prim String
     | Var TVar
     | Function Type Type
     deriving(Eq)

    typeInt::Type
    typeInt = Prim "Int"

    typeBool::Type
    typeBool = Prim "Bool"

    type Substitution = [(Type,TVar)]
    type Env = [(Id,TypeScheme)]
    
    instance Show Type where
        show (Prim s) = s
        show (Var v) = v 
        show (Function t1 t2) = 
            (show t1) ++ " -> " ++ (show t2)
    
    instance Show TypeScheme where
        show (ForAll l t) = 
            "∀" ++ (showListVar l) ++ ", " ++ (show t) 
        show (TP t) = show t 

    showListVar::[TVar] -> String
    showListVar (v:xs) = 
        (show v) ++ ", " ++ (showListVar xs)
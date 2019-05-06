module Syntax where


    type Id = String
    type TVar= String

    data Expr = Vari Id
     | App Expr Expr
     | Abs Id Expr
     | Let Id Expr Expr
     deriving(Show)

    data TypeScheme = ForAll [TVar] Type
    
    data Type = Prim String
     | Var TVar
     | Function Type Type
     deriving(Show,Eq)

    typeInt::Type
    typeInt = Prim "Int"

    typeBool::Type
    typeBool = Prim "Bool"
     
module Syntax where

type Id= String

data Term = Var Id
 | Lambda Id Term
 | App Term Term
 | Const Int
 | Term :+ Term
 | Term :- Term
 | Term :* Term
 | Term :/ Term
 | Ifzero Term Term Term
 | Let Id Term Term
 | Fix Term
 deriving(Show,Eq)

type Value = Term

--data Value = C Int
-- | V Id
-- | L Id Term
-- |Fixe Term	
 --deriving(Show,Eq)


--App (Fix (Lambda "f" (Lambda "x"))) (Const 3)
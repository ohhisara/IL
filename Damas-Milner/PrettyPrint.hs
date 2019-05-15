module PrettyPrint where
    import Syntax

    ppSubstitution::Substitution -> String
    ppSubstitution ((t,id):[]) = "[" ++ id ++ "/" ++ (show t) ++ "]"
    ppSubstitution ((t,id):xs) = "[" ++ id ++ "/" ++ (show t) ++ "], " ++ (ppSubstitution xs)

    pp::(Substitution,Type) -> String
    pp (s,t) = "Substitution: " ++ ppSubstitution s ++ " \n" ++  " Type: " ++ show t

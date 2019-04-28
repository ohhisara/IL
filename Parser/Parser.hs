module Parser where
    import Syntax

    {-main = do
        fileContent <- getContents
        putStr (map parse (lines fileContent))-}

    newtype Parser a = Parser (String ->  [(a,String)]) )

    run::Parser a -> String -> [(a,String)]
    run (Parser f) = f

    item :: Parser Char
    item = Parser (\cs -> case cs of
        "" -> []
        (c:cs) -> [(c,cs)])
    
    instance Monad Parser where
        return a = Parser (\cs -> [(a,cs)])
        p >>= f = Parser (\cs -> concat [parse (f a) cs’ | (a,cs’) <- parse p cs])
    
    empty::Parser a
    empty = Parser (\x -> [])

    (+++)::Parser a -> Parser a -> Parser a
    p (+++) q = Parser (\x -> (run Parser (p x)) ++ (run Parser (q x)))

    (<|>):: Parser a -> Parser a -> Parser a
    p (<|>) q = Parser (\x -> take 1 (run Parser (p+++q) x))

    satisfy::(Char -> Bool) -> Parser Char
    satisfy p = do 
        c <- item
        if (p c) then return c
        else empty
    
    char :: Char -> Parser Char
    char c = satisfy (c ==)

    string :: String -> Parser String
    string "" = return ""
    string (c:cs) = do {char c; string cs; return (c:cs)}

    many :: Parser a -> Parser [a]
    many p = many1 p +++ return []rm

    many1 :: Parser a -> Parser [a]
    many1 p = do {a <- p; as <- many p; return (a:as)}

    space :: Parser String
    space = many (satisfy isSpace)

    token :: Parser a -> Parser a
    token p = do {a <- p; space; return a}

    symb :: String -> Parser String
    symb cs = token (string cs)

    apply :: Parser a -> String -> [(a,String)]
    apply p = parse (do {space; p})

    var::Parser String
    var v= return v 

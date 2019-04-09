{
module Parse where
import Syntax
import Data.Char
}

%name syntax
%tokentype { Token }
%error { parseError }

%token
	let { TokenLet }
    in  { TokenIn }
    int { TokenInt $$ }
    var { TokenVar $$ }
    '\\' { TokenLam}
    '.' { TokenDot }
    '=' { TokenEq }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '(' { TokenOB }
    ')' { TokenCB }
    ifzero { TokenIf }
    fix {TokenFix}

%%
Term: int {Const $1}
	| var {Var $1}
	| '\\' var '.' Term {Lambda $2 $4}
	| '(' Term ')' '(' Term ')' {App $2 $5}
	| Term '+' Term {$1 :+ $3}
	| Term '-' Term {$1 :- $3}
	| Term '*' Term {$1 :* $3}
    | ifzero Term Term Term {Ifzero $2 $3 $4}
    | let var '=' Term in Term { Let $2 $4 $6 }
    | fix Term {Fix $2}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenLet
      | TokenIn
      | TokenInt Int
      | TokenVar String
      | TokenLam
      | TokenDot
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenOB
      | TokenCB
      | TokenIf
      | TokenFix
 	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer ('.':cs) = TokenDot : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      ("ifzero",rest)-> TokenIf : lexer rest
      ("fix",rest)-> TokenFix : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

parse::String->Term
parse s = syntax(lexer s)

}
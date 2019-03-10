module Instructions where

data Instr = LDC Int 
 | LD String
 | LDF Code
 | LDRF Code
 | AP 
 | RTN
 | SEL Code Code
 | JOIN
 | ADD
 | SUB
 | MUL
 | DIV
 | HALT
 deriving(Show)

type Code = [Instr]

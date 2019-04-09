module Instructions where

data Instr = LDC Int 
 | LD Int
 | LDF Code
 | LDRF Code
 | AP 
 | RTN
 | SEL Code Code
 | JOIN
 | ADD
 | SUB
 | MUL
 | HALT
 | DELAY Code
 deriving(Show)

type Code = [Instr]

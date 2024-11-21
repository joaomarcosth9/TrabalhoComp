module Token where

data Token
  = NUM Double
  | ADD
  | SUB
  | MUL
  | DIV
  | LPAR
  | RPAR
  | AND
  | OR
  | GTH
  | GEQTH
  | LTH
  | LEQTH
  | NOT
  | EQU
  | DIFF
  deriving (Eq, Show)
  

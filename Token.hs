module Token where

data Token
  = CDOUBLE Double
  | CINT Int
  | CSTRING String
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
  | IF
  | ELSE
  | WHILE
  | READ
  | PRINT
  | RETURN
  | LBRA
  | RBRA
  | COM
  | ECOM
  | ID String
  | TINT
  | TDOUBLE
  | TSTRING
  | TVOID
  | ASSING
  
  
  deriving (Eq, Show)
  

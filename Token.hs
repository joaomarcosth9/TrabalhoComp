module Token where

data Token
  = CDOUBLE Double
  | CINT Int
  | CLITERAL String
  | ID String

  | ADD
  | SUB
  | MUL
  | DIV

  | LPAR
  | RPAR
  | LBRA
  | RBRA

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

  | COMMA
  | SEMI

  | TINT
  | TDOUBLE
  | TSTRING
  | TVOID
  | ASSIGN
  
  deriving (Eq, Show)
  

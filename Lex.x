{
module Lex where

import Token
}

%wrapper "basic"

$digit = [0-9]          -- digits
$letter = [a-zA-Z]      -- letters
@id = ($letter | \_)($letter | $digit | \_)* -- identifier
@int = $digit+
@double = $digit+(\.$digit+)? -- double
@literal = \"[^\"]*\"     -- string

tokens :-

<0> "+" {\s -> ADD}  
<0> "-" {\s -> SUB}  
<0> "*" {\s -> MUL}  
<0> "/" {\s -> DIV}  

<0> "(" {\s -> LPAR}  
<0> ")" {\s -> RPAR}  
<0> "{" {\s -> LBRA}
<0> "}" {\s -> RBRA}

<0> "&&" {\s -> AND}
<0> "||" {\s -> OR}
<0> ">" {\s -> GTH}
<0> ">=" {\s -> GEQTH}
<0> "<" {\s -> LTH}
<0> "<=" {\s -> LEQTH}

<0> "!" {\s -> NOT}
<0> "==" {\s -> EQU}
<0> "!=" {\s -> DIFF}

<0> "if" {\s -> IF}
<0> "else" {\s -> ELSE}
<0> "while" {\s -> WHILE}
<0> "read" {\s -> READ}
<0> "print" {\s -> PRINT}
<0> "return" {\s -> RETURN}

<0> "," {\s -> COMMA}
<0> ";" {\s -> SEMI}

<0> "int" {\s -> TINT}
<0> "double" {\s -> TDOUBLE}
<0> "string" {\s -> TSTRING}
<0> "void" {\s -> TVOID}
<0> "=" {\s -> ASSIGN}

<0> $white+ ;
<0> @int { \s -> CINT (read s) }
<0> @double { \s -> CDOUBLE (read s) }
<0> @literal { \s -> CLITERAL (init (drop 1 s)) }
<0> @id { \s -> ID s }

{
main = do 
            s <- readFile "in"
            print (alexScanTokens s)
}

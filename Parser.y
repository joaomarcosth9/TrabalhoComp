{
module Parser where

import Token
import qualified Lex as L

}

%name calc
%tokentype { Token }
%error { parseError }
%token 
  '+' {ADD}
  '-' {SUB}
  '*' {MUL}
  '/' {DIV}
  '(' {LPAR}
  ')' {RPAR}
  '&&' {AND}
  '||' {OR}
  '>' {GTH}
  '>=' {GEQTH}
  '<' {LTH}
  '<=' {LEQTH}
  '!' {NOT}
  '==' {EQU}
  '!=' {DIFF}
  Num {NUM $$}


%%

BoolOrExpr : BoolOrExpr '||' BoolExpr  {$1 ++ " " ++ $3 ++ " ||"}
           | BoolExpr                  {$1}

BoolExpr : BoolExpr '&&' BoolTerm      {$1 ++ " " ++ $3 ++ " &&"}
         | '!' BoolTerm                {"! " ++ $2}
         | BoolTerm                    {$1}

BoolTerm : Expr '>' Expr               {show $1 ++ " " ++ show $3 ++ " >"}
         | Expr '==' Expr              {show $1 ++ " " ++ show $3 ++ " =="}
         | Expr '!=' Expr              {show $1 ++ " " ++ show $3 ++ " !="}
         | '(' BoolExpr ')'            {$2}

Expr  : Expr '+' Term       {$1 + $3}
      | Expr '-' Term       {$1 - $3}
      | Term                {$1}

Term  : Term '*' Factor    {$1 * $3}
      | Term '/' Factor     {$1 / $3}
      | Factor              {$1}

Factor : Num                {$1}
       | '(' Expr ')'       {$2}      


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do 
          s <- readFile "in"
          print (calc (L.alexScanTokens s))
}

{
module Parser where

import Token
import qualified Lex as L
import ASTree

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
  '{' {LBRA}
  '}' {RBRA}
  '&&' {AND}
  '||' {OR}
  '>' {GTH}
  '>=' {GEQTH}
  '<' {LTH}
  '<=' {LEQTH}
  '!' {NOT}
  '==' {EQU}
  '!=' {DIFF}
  'if' {IF}
  'else' {ELSE}
  'while' {WHILE}
  'read' {READ}
  'print' {PRINT}
  'return' {RETURN}
  ',' {COMMA}
  ';' {SEMI}
  'int' {TINT}
  'double' {TDOUBLE}
  'string' {TSTRING}
  'void' {TVOID}
  '=' {ASSIGN}
  Int {CINT $$}
  Double {CDOUBLE $$}
  Literal {CLITERAL $$}
  Name {ID $$}

%%

Program : FunctionList MainBlock { Prog (map fst $1) (map aux $1) (fst $2) (snd $2) }
        | MainBlock              { Prog [] [] (fst $1) (snd $1) }

FunctionList : FunctionList Function {$1 ++ [$2]}
             | Function              {[$1]}

Function : ReturnType Name '(' DeclParameterList ')' MainBlock { ($2 :->: ($4, $1), $6) }
         | ReturnType Name '(' ')' MainBlock                   { ($2 :->: ([], $1), $5) }

ReturnType : Tipo {$1}
           | 'void' {TVoid}

DeclParameterList : DeclParameterList ',' DeclParameter    {$1 ++ [$3]}
                  | DeclParameter                          {[$1]}

DeclParameter : Tipo Name { $2 :#: ($1, 0) }

MainBlock : '{' DeclarationList CommandList '}'         {($2, $3)}
          | '{' CommandList '}'                         {([], $2)}

DeclarationList : DeclarationList Declaration   {$1 ++ $2}
                | Declaration                   {$1}

Declaration : Tipo NameList ';' {map (\x -> x :#: ($1, 0)) $2}

Tipo : 'int' {TInt}
     | 'double' {TDouble}
     | 'string' {TString}

NameList : NameList ',' Name  {$1 ++ [$3]}
         | Name               {[$1]}

Block : '{' CommandList '}' {$2}

CommandList : CommandList Command    {$1 ++ [$2]}
            | Command                {[$1]}

Command : IfCommand {$1}
        | WhileCommand {$1}
        | AttribCommand {$1}
        | WriteCommand {$1}
        | ReadCommand {$1}
        | ProcCall {$1}
        | Return {$1}


Return : 'return' AritExpr ';' {Ret (Just $2)}
--        | 'return' Literal ';' {Ret (Just (Lit $2))}
       | 'return' ';' {Ret (Nothing)}

IfCommand : 'if' '(' LogicalExpr ')' Block {If $3 $5 []}
          | 'if' '(' LogicalExpr ')' Block 'else' Block {If $3 $5 $7}

WhileCommand : 'while' '(' LogicalExpr ')' Block {While $3 $5}

AttribCommand : Name '=' AritExpr ';' {Atrib $1 $3}
        --       | Name '=' Literal ';' {Atrib $1 (Lit $3)}

WriteCommand : 'print' '(' AritExpr ')' ';' {Imp $3}
        --      | 'print' '(' Literal ')' ';' {Imp (Lit $3)}

ReadCommand : 'read' '(' Name ')' ';' {Leitura $3}

ProcCall : FunctionCall ';' {Proc (fst $1) (snd $1)}

FunctionCall : Name '(' FunctionParameterList ')'   {($1, $3)}
             | Name '(' ')'                 {($1, [])}

FunctionParameterList : FunctionParameterList ',' AritExpr        {$1 ++ [$3]}
                      | AritExpr                          {[$1]}
                --       | FunctionParameterList ',' Literal     {$1 ++ [Lit $3]}
                --       | Literal                       {[Lit $1]}

LogicalExpr : LogicalExpr '||' LogicalTerm  {Or $1 $3}
            | LogicalExpr '&&' LogicalTerm  {And $1 $3}
            | LogicalTerm             {$1}

LogicalTerm : LogicalFactor              {$1}
            | '!' LogicalTerm            {Not $2}

LogicalFactor : '(' LogicalExpr ')'         {$2}
              | RelationalExpr              {Rel $1}

RelationalExpr : AritExpr '>' AritExpr               {Rgt $1 $3}
               | AritExpr '>=' AritExpr              {Rge $1 $3}
               | AritExpr '<' AritExpr               {Rlt $1 $3}
               | AritExpr '<=' AritExpr              {Rle $1 $3}
               | AritExpr '==' AritExpr              {Req $1 $3}
               | AritExpr '!=' AritExpr              {Rdif $1 $3}

AritExpr  : AritExpr '+' Term       {Add $1 $3}
          | AritExpr '-' Term       {Sub $1 $3}
          | Term                    {$1}

Term  : Term '*' Factor     {Mul $1 $3}
      | Term '/' Factor     {Div $1 $3}
      | Factor              {$1}

Factor : Int                {Const (CInt $1)}
       | Double             {Const (CDouble $1)}
       | Name               {IdVar $1}
       | Literal            {Lit $1}
       | FunctionCall       {Chamada (fst $1) (snd $1)}
       | '(' AritExpr ')'   {$2}      
       | '-' Factor         {Neg $2}

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

genAST :: String -> Programa
genAST text = calc (L.alexScanTokens text)

run = do 
        s <- readFile "in"
        print (genAST s)

main = do 
        txt <- getLine
        s <- readFile txt
        print (genAST s)
}




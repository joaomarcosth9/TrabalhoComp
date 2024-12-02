module Semantic where

import qualified Parser as P

import ASTree
import Control.Concurrent.STM (check)

-- Definição da Mônada M
data M a = MS (String, a) deriving Show

instance Functor M where
    fmap f (MS (s, a)) = MS (s, f a)

instance Applicative M where
    pure a = MS ("", a)
    MS (s1, f) <*> MS (s2, x) = MS (s1 ++ s2, f x)

instance Monad M where
    MS (s1, a) >>= f = let MS (s2, b) = f a in MS (s1 ++ s2, b)

emiteErro :: String -> M ()
emiteErro msg = MS ("Erro: " ++ msg ++ "\n", ())

emiteAviso :: String -> M ()
emiteAviso msg = MS ("Aviso: " ++ msg ++ "\n", ())

-- Conta as ocorrencias de um Id em uma lista de Ids
countId :: Id -> [Id] -> Int
countId _ [] = 0
countId id (x : xs)
    | id == x = 1 + countId id xs
    | otherwise = countId id xs

--checka funções


checkVariaveis' :: [Var] -> [Id] -> Id -> M [Var]
checkVariaveis' [] _ _ = pure []
checkVariaveis' ((id :#: (tipo, _)) : vs) declared escopo
    | countId id declared == 1 = do
        emiteErro ("Variavel '" ++ id ++ "' declarada multiplas vezes em " ++ escopo ++ ".")
        checkVariaveis' vs (declared ++ [id]) escopo
    | countId id declared > 1 = do
        checkVariaveis' vs (declared ++ [id]) escopo
    | otherwise = do
        vs' <- checkVariaveis' vs (declared ++ [id]) escopo
        pure ((id :#: (tipo, 0)) : vs')

-- Verifica se as variaveis em um Bloco foram declaradas uma unica vez
checkVariaveis :: [Var] -> Id -> M [Var]
checkVariaveis vars escopo = checkVariaveis' vars [] escopo

    -- | Atrib Id Expr
    -- | Leitura Id
    -- | Imp Expr
    -- | Ret (Maybe Expr)
    -- | Proc Id [Expr]

checkExpr :: [Funcao] -> [Var] -> Expr -> M Expr
checkExpr funcoes _ (Cint n) = pure (Cint n)
checkExpr funcoes _ (Cdouble n) = pure (Cdouble n)
checkExpr funcoes _ (Lit s) = pure (Lit s)

checkExpr funcoes vars (Var id) = do
    case lookup id vars of
        Just (tipo, _) -> pure (Var id)
        Nothing -> do
            emiteErro ("Variavel '" ++ id ++ "' nao declarada.")
            pure (Var id)

checkExpr funcoes vars (Neg expr) = do
    expr' <- checkExpr funcoes vars expr
    if( expr' == TString) then do
        emiteErro ("Operador de negacao nao pode ser aplicado a uma string.")
        pure (Neg expr')
    else
    pure (Neg expr')    

checkExpr funcoes vars (IntDouble expr) = do
    expr' <- checkExpr funcoes vars expr
    if( expr' == Tint) then
        pure (IntDouble expr')
        else do
            emiteErro ("Operador de conversao de inteiro para double nao pode ser aplicado.")
            pure (IntDouble expr')


checkExpr funcoes vars (DoubleInt expr) = do
    expr' <- checkExpr funcoes vars expr
    if( expr' == Tdouble) then
        pure (DoubleInt expr')
        else do
            emiteErro ("Operador de conversao de double para inteiro nao pode ser aplicado.")
            pure (DoubleInt expr')

checkExpr funcoes vars (Add expr1 expr2) = do
    expr1' <- checkExpr funcoes vars expr1
    expr2' <- checkExpr funcoes vars expr2
    if (expr1' == TString || expr2' == TString) then do
        emiteErro ("Operador de adicao nao pode ser aplicado a strings.")
        pure (Add expr1' expr2')
    else if (expr1' == TInt && expr2' == TDouble) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Add (IntDouble expr1') expr2')
    else if (expr1' == TDouble && expr2' == TInt) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Add expr1' (IntDouble expr2'))
    else
        pure (Add expr1' expr2')
    
checkExpr funcoes vars (Sub expr1 expr2) = do
    expr1' <- checkExpr funcoes vars expr1
    expr2' <- checkExpr funcoes vars expr2
    if (expr1' == TString || expr2' == TString) then do
        emiteErro ("Operador de subtracao nao pode ser aplicado a strings.")
        pure (Sub expr1' expr2')
    else if (expr1' == TInt && expr2' == TDouble) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Sub (IntDouble expr1') expr2')
    else if (expr1' == TDouble && expr2' == TInt) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Sub expr1' (IntDouble expr2'))
    else
        pure (Sub expr1' expr2')
    
checkExpr funcoes vars (Mul expr1 expr2) = do
    expr1' <- checkExpr funcoes vars expr1
    expr2' <- checkExpr funcoes vars expr2
    if (expr1' == TString || expr2' == TString) then do
        emiteErro ("Operador de multiplicacao nao pode ser aplicado a strings.")
        pure (Mul expr1' expr2')
    else if (expr1' == TInt && expr2' == TDouble) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Mul (IntDouble expr1') expr2')
    else if (expr1' == TDouble && expr2' == TInt) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Mul expr1' (IntDouble expr2'))
    else
        pure (Mul expr1' expr2')
        
checkExpr funcoes vars (Div expr1 expr2) = do
    expr1' <- checkExpr funcoes vars expr1
    expr2' <- checkExpr funcoes vars expr2
    if (expr1' == TString || expr2' == TString) then do
        emiteErro ("Operador de divisao nao pode ser aplicado a strings.")
        pure (Div expr1' expr2')
    else if (expr1' == TInt && expr2' == TDouble) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Div (IntDouble expr1') expr2')
    else if (expr1' == TDouble && expr2' == TInt) then do
        emiteAviso ("Inteiro sendo convertido para double.")
        pure (Div expr1' (IntDouble expr2'))
    else
        pure (Div expr1' expr2')


checkExpr funcoes vars (Chamada id exprs) = do
    case lookup id funcoes of
        Just (tipo, _) -> do
            exprs' <- mapM (checkExpr funcoes vars) exprs
            pure (Chamada id exprs')
        Nothing -> do
            emiteErro ("Funcao '" ++ id ++ "' nao declarada.")
            pure (Chamada id exprs)

-- Dudu ele cria o checkExpr com um parametro a mais, que é o tipo do operador ai ele faz um teste a mais que é checkExpr _ _ expr 
-- Eu fiz separado 
-- Espero que esteja certo :D 


checkExprR :: [Funcao] -> ExprR -> M ExprR
checkExprR funcoes (Req expr1 expr2) = do
    expr1' <- checkExpr funcoes expr1
    expr2' <- checkExpr funcoes expr2
    pure (Req expr1' expr2')

checkExprR funcoes (Rdif expr1 expr2) = do
    expr1' <- checkExpr funcoes expr1
    expr2' <- checkExpr funcoes expr2
    pure (Rdif expr1' expr2')

checkExprR funcoes (Rlt expr1 expr2) = do
    expr1' <- checkExpr funcoes expr1
    expr2' <- checkExpr funcoes expr2
    pure (Rlt expr1' expr2')

checkExprR funcoes (Rgt expr1 expr2) = do
    expr1' <- checkExpr funcoes expr1
    expr2' <- checkExpr funcoes expr2
    pure (Rgt expr1' expr2')

checkExprR funcoes (Rle expr1 expr2) = do
    expr1' <- checkExpr funcoes expr1
    expr2' <- checkExpr funcoes expr2
    pure (Rle expr1' expr2')

checkExprR funcoes (Rge expr1 expr2) = do
    expr1' <- checkExpr funcoes expr1
    expr2' <- checkExpr funcoes expr2
    pure (Rge expr1' expr2')

checkExprL :: ExprL -> M ExprL
checkExprL (And exprL1 exprL2) = do
    exprL1' <- checkExprL exprL1
    exprL2' <- checkExprL exprL2
    pure (And exprL1' exprL2')

checkExprL (Or exprL1 exprL2) = do
    exprL1' <- checkExprL exprL1
    exprL2' <- checkExprL exprL2
    pure (Or exprL1' exprL2')

checkExprL (Not exprL) = do
    exprL' <- checkExprL exprL
    pure (Not exprL')

checkExprL (Rel exprR) = do
    exprR' <- checkExprR exprR
    pure (Rel exprR')

checkComando :: [Funcao] -> Comando -> M Comando
checkComando funcoes (If exprL bloco1 bloco2) = do
    bloco1' <- checkBloco funcoes bloco1
    bloco2' <- checkBloco funcoes bloco2
    exprL <- checkExprL exprL
    pure (If exprL bloco1' bloco2')

checkComando funcoes (While exprL bloco) = do
    bloco' <- checkBloco funcoes bloco
    exprL' <- checkExprL exprL
    pure (While exprL' bloco')

checkComando funcoes (Atrib id expr) = do
    expr' <- checkExpr funcoes expr
    -- tem que verificar mais coisas
    case lookup id vars of
        Just (tipo, _) -> do
            if (tipo == TString && expr' /= TString) then do
                emiteErro ("Atribuicao de tipo invalido para variavel '" ++ id ++ "'.")
                pure (Atrib id expr')
            else if (tipo == Tint && expr' == TDouble) then do
                emiteAviso ("Double sendo convertido para inteiro.")
                pure (Atrib id (DoubleInt expr'))
            else if(tipo == Tdouble && expr' == TInt) then do
                emiteAviso ("Inteiro sendo convertido para double.")
                pure (Atrib id (IntDouble expr'))
            else if(tipo == Tint && expr' /= Tint) then do
                emiteErro ("Atribuicao de tipo invalido para variavel '" ++ id ++ "'.")
                pure (Atrib id expr')
            else if(tipo == Tdouble && expr' /= Tdouble) then do
                emiteErro ("Atribuicao de tipo invalido para variavel '" ++ id ++ "'.")
                pure (Atrib id expr')
            else
                pure (Atrib id expr')
        Nothing -> do
            emiteErro ("Variavel '" ++ id ++ "' nao declarada.")
    -- Copilot cantou aqui, comparei com o dudu e ele fez bastante parecido, só que ele sempre separa o monada ali ele faz tipo (eType, new_expr)
    -- nao sei porque ele faz isso
    
    pure (Atrib id expr')

checkComando funcoes (Leitura id) = do
    let tipo = case lookup id vars of
        Just (tipo, _) -> tipo
        Nothing -> do
            emiteErro ("Variavel '" ++ id ++ "' nao declarada.")
            TString
            -- nao entendi porque o copilot sugeriu isso 
    

checkComando funcoes (Imp expr) = do
    expr' <- checkExpr funcoes expr
    pure (Imp expr')

checkComando funcoes (Ret maybeExpr) = do
    maybeExpr' <- case maybeExpr of
        Just expr -> do
            expr' <- checkExpr funcoes expr
            pure (Just expr')
        Nothing -> pure Nothing
    pure (Ret maybeExpr')

checkComando funcoes (Proc id exprs) = do
    exprs' <- mapM (checkExpr funcoes) exprs
    --- acho que aqui ainda tem que verificar mais coisas
    pure (Proc id exprs')

checkBloco :: [Funcao] -> Bloco -> M Bloco
checkBloco funcoes [] = pure []
checkBloco funcoes (c : cs) = do
    c' <- checkComando funcoes c
    cs' <- checkBloco funcoes cs
    pure (c' : cs')

checkAST :: Programa -> M Programa
checkAST (Prog funcoes corpoFuncoes variaveisBlocoPrincipal blocoPrincipal) = do
    -- Verificar se as funções estão corretas
    -- funcoes' <- checkFuncoes funcoes
    -- Verificar se as variáveis estão corretas
    -- variaveis' <- checkVariaveis variaveis
    -- Verificar se o bloco principal está correto
    variaveisBlocoPrincipal' <- checkVariaveis variaveisBlocoPrincipal "Bloco Principal"
    blocoPrincipal' <- checkBloco funcoes blocoPrincipal
    pure (Prog funcoes corpoFuncoes variaveisBlocoPrincipal' blocoPrincipal')

main = do
    putStrLn "Rodando analisador semantico"
    arquivo <- getLine
    conteudo <- readFile arquivo
    let ast = P.genAST conteudo
    putStrLn "Arvore antes: "
    print ast
    putStrLn "Arvore depois: "
    print (checkAST ast)


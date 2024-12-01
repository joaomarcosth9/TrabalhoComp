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

checkExpr :: [Funcao] -> Expr -> M Expr

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
    pure (Atrib id expr')

checkComando funcoes (Leitura id) = pure (Leitura id)

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


module Semantic where

import ASTree
import Control.Concurrent.STM (check)
import Parser qualified as P

-- Definição da Mônada M
data M a = MS (String, a) deriving (Show)

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
contaId :: Id -> [Id] -> Int
contaId _ [] = 0
contaId id (x : xs)
  | id == x = 1 + contaId id xs
  | otherwise = contaId id xs

varExiste :: Id -> [Var] -> Bool
varExiste _ [] = False
varExiste id ((id' :#: _) : vs)
  | id == id' = True
  | otherwise = varExiste id vs

tipoVar :: Id -> [Var] -> Tipo
tipoVar id ((id' :#: (tipo, _)) : vs)
  | id == id' = tipo
  | otherwise = tipoVar id vs

funcaoExiste :: Id -> [Funcao] -> Bool
funcaoExiste _ [] = False
funcaoExiste id ((id' :->: _) : fs)
  | id == id' = True
  | otherwise = funcaoExiste id fs

retornoFuncao :: Id -> [Funcao] -> Tipo
retornoFuncao id ((id' :->: (_, tipo)) : fs)
  | id == id' = tipo
  | otherwise = retornoFuncao id fs

numeroParamFuncao :: Id -> [Funcao] -> Int
numeroParamFuncao id ((id' :->: (params, _)) : fs)
  | id == id' = length params
  | otherwise = numeroParamFuncao id fs

listaTiposParamFuncao :: Id -> [Funcao] -> [Tipo]
listaTiposParamFuncao id ((id' :->: (params, _)) : fs)
  | id == id' = map (\(_ :#: (tipo, _)) -> tipo) params
  | otherwise = listaTiposParamFuncao id fs

checkFuncoes' :: [Funcao] -> [Id] -> M [Funcao]
checkFuncoes' [] _ = pure []
checkFuncoes' ((id :->: (params, tipo)) : fs) declaradas
  | contaId id declaradas >= 1 = do
      emiteErro ("Funcao '" ++ id ++ "' declarada multiplas vezes em: " ++ show (id :->: (params, tipo)))
      checkFuncoes' fs (declaradas ++ [id])
  | otherwise = do
      fs' <- checkFuncoes' fs (declaradas ++ [id])
      pure ((id :->: (params, tipo)) : fs')

checkFuncoes :: [Funcao] -> M [Funcao]
checkFuncoes funcoes = checkFuncoes' funcoes []

checkVariaveis' :: [Var] -> [Id] -> M [Var]
checkVariaveis' [] _ = pure []
checkVariaveis' ((id :#: (tipo, _)) : vs) declaradas
  | contaId id declaradas >= 1 = do
      emiteErro ("Variavel '" ++ id ++ "' declarada multipas vezes em: " ++ show (id :#: (tipo, 0)))
      checkVariaveis' vs (declaradas ++ [id])
  | otherwise = do
      vs' <- checkVariaveis' vs (declaradas ++ [id])
      pure ((id :#: (tipo, 0)) : vs')

-- Verifica se as variaveis em um Bloco foram declaradas uma unica vez
checkVariaveis :: [Var] -> M [Var]
checkVariaveis vars = checkVariaveis' vars []

-- \| Atrib Id Expr
-- \| Leitura Id
-- \| Imp Expr
-- \| Ret (Maybe Expr)
-- \| Proc Id [Expr]

--     data TCons = CDouble Double
--            | CInt Int
--            deriving Show

-- data Expr =
--           | Chamada Id [Expr]
--           | IntDouble Expr
--           | DoubleInt Expr
--           deriving Show

checkExprTipos :: [Funcao] -> [Var] -> Expr -> Expr -> (Expr -> Expr -> Expr) -> M (Tipo, Expr)
checkExprTipos funcoes vars expr1 expr2 op = do
  (tipo1, expr1') <- checkExpr funcoes vars expr1
  (tipo2, expr2') <- checkExpr funcoes vars expr2
  if (tipo1 == TString || tipo2 == TString)
    then do
      emiteErro ("Operador nao pode ser aplicado a strings.")
      pure (TVoid, op expr1' expr2')
    else
      if (tipo1 == TInt && tipo2 == TDouble)
        then do
          emiteAviso ("Inteiro sendo convertido para double.")
          pure (TDouble, op (IntDouble expr1') expr2')
        else
          if (tipo1 == TDouble && tipo2 == TInt)
            then do
              emiteAviso ("Inteiro sendo convertido para double.")
              pure (TDouble, op expr1' (IntDouble expr2'))
            else
              pure (tipo1, op expr1' expr2')

checkExpr :: [Funcao] -> [Var] -> Expr -> M (Tipo, Expr)
checkExpr funcoes vars (Const (CDouble a)) = pure (TDouble, Const (CDouble a))
checkExpr funcoes vars (Const (CInt a)) = pure (TInt, Const (CInt a))
checkExpr funcoes vars (Lit a) = pure (TString, Lit a)
checkExpr funcoes vars (IdVar id)
  | varExiste id vars = pure (tipoVar id vars, IdVar id)
  | otherwise = do
      emiteErro ("Variavel '" ++ id ++ "' nao declarada.")
      pure (TVoid, IdVar id)
checkExpr funcoes vars (Neg expr) = do
  (tipo, expr') <- checkExpr funcoes vars expr
  if (tipo == TInt || tipo == TDouble)
    then
      pure (tipo, Neg expr')
    else do
      emiteErro ("Nao e possivel negativar uma string.")
      pure (TVoid, Neg expr')
checkExpr funcoes vars (Add expr1 expr2) = do checkExprTipos funcoes vars expr1 expr2 Add
checkExpr funcoes vars (Sub expr1 expr2) = do checkExprTipos funcoes vars expr1 expr2 Sub
checkExpr funcoes vars (Mul expr1 expr2) = do checkExprTipos funcoes vars expr1 expr2 Mul
checkExpr funcoes vars (Div expr1 expr2) = do checkExprTipos funcoes vars expr1 expr2 Div
checkExpr funcoes vars (IntDouble expr) = do
  (tipo, expr') <- checkExpr funcoes vars expr
  if (tipo == TInt)
    then
      pure (TDouble, IntDouble expr')
    else do
      emiteErro ("Nao e possivel converter uma string para double.")
      pure (TVoid, IntDouble expr')
checkExpr funcoes vars (DoubleInt expr) = do
  (tipo, expr') <- checkExpr funcoes vars expr
  if (tipo == TDouble)
    then
      pure (TInt, DoubleInt expr')
    else do
      emiteErro ("Nao e possivel converter uma string para inteiro.")
      pure (TVoid, DoubleInt expr')
checkExpr funcoes vars (Chamada id exprs) = do
  if not (funcaoExiste id funcoes)
    then do
      emiteErro ("Funcao '" ++ id ++ "' nao declarada.")
      pure (TVoid, Chamada id exprs)
    else do
      let n = numeroParamFuncao id funcoes
      if n /= length exprs
        then do
          emiteErro ("Numero de parametros incorreto para a funcao '" ++ id ++ "'.")
          pure (TVoid, Chamada id exprs)
        else do
          let tipos = listaTiposParamFuncao id funcoes
          exprs' <- mapM (checkExpr funcoes vars) exprs
          let tipos' = map fst exprs'
          if tipos /= tipos'
            then do
              emiteErro ("Tipos de parametros incorretos para a funcao '" ++ id ++ "'.")
              pure (TVoid, Chamada id exprs)
            else
              pure (retornoFuncao id funcoes, Chamada id exprs)

checkExprRTipos :: [Funcao] -> [Var] -> Expr -> Expr -> (Expr -> Expr -> ExprR) -> M (Tipo, ExprR)
checkExprRTipos funcoes vars expr1 expr2 op = do
  (tipo1, expr1') <- checkExpr funcoes vars expr1
  (tipo2, expr2') <- checkExpr funcoes vars expr2
  if (tipo1 == TString || tipo2 == TString)
    then do
      emiteErro ("Operador nao pode ser aplicado a strings.")
      pure (TVoid, op expr1' expr2')
    else
      if (tipo1 == TInt && tipo2 == TDouble)
        then do
          emiteAviso ("Inteiro sendo convertido para double.")
          pure (TDouble, op (IntDouble expr1') expr2')
        else
          if (tipo1 == TDouble && tipo2 == TInt)
            then do
              emiteAviso ("Inteiro sendo convertido para double.")
              pure (TDouble, op expr1' (IntDouble expr2'))
            else
              pure (tipo1, op expr1' expr2')

checkExprR :: [Funcao] -> [Var] -> ExprR -> M ExprR
checkExprR funcoes vars (Req expr1 expr2) = do
  (_, exprs) <- checkExprRTipos funcoes vars expr1 expr2 Req
  pure exprs
checkExprR funcoes vars (Rdif expr1 expr2) = do
  (_, exprs) <- checkExprRTipos funcoes vars expr1 expr2 Rdif
  pure exprs
checkExprR funcoes vars (Rlt expr1 expr2) = do
  (_, exprs) <- checkExprRTipos funcoes vars expr1 expr2 Rlt
  pure exprs
checkExprR funcoes vars (Rgt expr1 expr2) = do
  (_, exprs) <- checkExprRTipos funcoes vars expr1 expr2 Rgt
  pure exprs
checkExprR funcoes vars (Rle expr1 expr2) = do
  (_, exprs) <- checkExprRTipos funcoes vars expr1 expr2 Rle
  pure exprs
checkExprR funcoes vars (Rge expr1 expr2) = do
  (_, exprs) <- checkExprRTipos funcoes vars expr1 expr2 Rge
  pure exprs

checkExprL :: [Funcao] -> [Var] -> ExprL -> M ExprL
checkExprL funcoes vars (And expr1 expr2) = do
  expr1' <- checkExprL funcoes vars expr1
  expr2' <- checkExprL funcoes vars expr2
  pure (And expr1' expr2')
checkExprL funcoes vars (Or expr1 expr2) = do
  expr1' <- checkExprL funcoes vars expr1
  expr2' <- checkExprL funcoes vars expr2
  pure (Or expr1' expr2')
checkExprL funcoes vars (Not exprL) = do
  exprL' <- checkExprL funcoes vars exprL
  pure (Not exprL')
checkExprL funcoes vars (Rel exprR) = do
  exprR' <- checkExprR funcoes vars exprR
  pure (Rel exprR')

checkComando :: [Funcao] -> [Var] -> Maybe Funcao -> Comando -> M Comando
checkComando funcoes vars funcao (If exprL bloco1 bloco2) = do
  bloco1' <- checkBloco funcoes vars funcao bloco1
  bloco2' <- checkBloco funcoes vars funcao bloco2
  exprL' <- checkExprL funcoes vars exprL
  pure (If exprL' bloco1' bloco2')
checkComando funcoes vars funcao (While exprL bloco) = do
  bloco' <- checkBloco funcoes vars funcao bloco
  exprL' <- checkExprL funcoes vars exprL
  pure (While exprL' bloco')
checkComando funcoes vars _ (Atrib id expr) = do
  expr' <- checkExpr funcoes vars expr
  if not (varExiste id vars)
    then do
      emiteErro ("Variavel '" ++ id ++ "' nao declarada.")
      pure (Atrib id (snd expr'))
    else do
      let tipo = tipoVar id vars
      let tipoexpr = fst expr'
      if tipoexpr == TVoid
        then do
          emiteErro ("Ocorreu algum erro na verificacao do tipo da expressao.")
          pure (Atrib id (snd expr'))
        else
          if tipo == tipoexpr
            then do
              pure (Atrib id (snd expr'))
            else
              if tipo == TDouble && tipoexpr == TInt
                then do
                  emiteAviso ("Inteiro sendo convertido para double.")
                  pure (Atrib id (IntDouble (snd expr')))
                else
                  if tipo == TInt && tipoexpr == TDouble
                    then do
                      emiteAviso ("Double sendo convertido para inteiro.")
                      pure (Atrib id (DoubleInt (snd expr')))
                    else do
                      emiteErro ("Atribuicao de tipo incorreto para a variavel '" ++ id ++ "'.")
                      pure (Atrib id (snd expr'))
checkComando funcoes vars _ (Leitura id) = do
  if not (varExiste id vars)
    then do
      emiteErro ("Variavel '" ++ id ++ "' nao declarada.")
      pure (Leitura id)
    else do
      pure (Leitura id)
checkComando funcoes vars _ (Imp expr) = do
  expr' <- checkExpr funcoes vars expr
  pure (Imp (snd expr'))
checkComando funcoes vars maybeFunc (Ret maybeExpr) = do
  case maybeFunc of
    Just (id :->: (_, tipo)) -> do
      case maybeExpr of
        Just expr -> do
          expr' <- checkExpr funcoes vars expr
          if tipo == TVoid
            then do
              emiteErro ("Funcao '" ++ id ++ "' nao deveria retornar nada.")
              pure (Ret maybeExpr)
            else do
              if tipo == fst expr'
                then do
                  pure (Ret maybeExpr)
                else
                  if tipo == TDouble && fst expr' == TInt
                    then do
                      emiteAviso ("Inteiro sendo convertido para double.")
                      pure (Ret (Just (IntDouble (snd expr'))))
                    else
                      if tipo == TInt && fst expr' == TDouble
                        then do
                          emiteAviso ("Double sendo convertido para inteiro.")
                          pure (Ret (Just (DoubleInt (snd expr'))))
                        else do
                          emiteErro ("Retorno de tipo incorreto para a funcao '" ++ id ++ "'.")
                          pure (Ret maybeExpr)
        Nothing -> do
          if tipo /= TVoid
            then do
              emiteErro ("Funcao '" ++ id ++ "' deveria retornar algo.")
              pure (Ret maybeExpr)
            else do
              pure (Ret maybeExpr)
    Nothing -> do
      case maybeExpr of
        Just expr -> do
          emiteErro ("Retorno de expressao em bloco principal.")
          pure (Ret maybeExpr)
        Nothing -> do
          pure (Ret maybeExpr)
checkComando funcoes vars _ (Proc id exprs) = do
  if not (funcaoExiste id funcoes)
    then do
      emiteErro ("Funcao '" ++ id ++ "' nao declarada.")
      pure (Proc id exprs)
    else do
      let n = numeroParamFuncao id funcoes
      if n /= length exprs
        then do
          emiteErro ("Numero de parametros incorreto para a funcao '" ++ id ++ "'.")
          pure (Proc id exprs)
        else do
          let tipos = listaTiposParamFuncao id funcoes
          exprs' <- mapM (checkExpr funcoes vars) exprs
          let tipos' = map fst exprs'
          if tipos /= tipos'
            then do
              emiteErro ("Tipos de parametros incorretos para a funcao '" ++ id ++ "'.")
              pure (Proc id exprs)
            else
              pure (Proc id exprs)

checkBloco :: [Funcao] -> [Var] -> Maybe Funcao -> Bloco -> M Bloco
checkBloco _ _ _ [] = pure []
checkBloco funcoes vars funcao (c : cs) = do
  c' <- checkComando funcoes vars funcao c
  cs' <- checkBloco funcoes vars funcao cs
  pure (c' : cs')

checkCorpoFuncoes :: [Funcao] -> [Funcao] -> [(Id, [Var], Bloco)] -> M [(Id, [Var], Bloco)]
checkCorpoFuncoes _ _ [] = pure []
checkCorpoFuncoes todasfuncoes declaradas ((id, vars, bloco) : corpoFuncoes) = do
  let tipo = retornoFuncao id todasfuncoes
  bloco' <- checkBloco declaradas vars (Just (id :->: (vars, tipo))) bloco
  corpoFuncoes' <- checkCorpoFuncoes todasfuncoes (declaradas ++ [id :->: (vars, tipo)]) corpoFuncoes
  pure ((id, vars, bloco') : corpoFuncoes')

checkAST :: Programa -> M Programa
checkAST (Prog funcoes corpoFuncoes variaveisBlocoPrincipal blocoPrincipal) = do
  funcoes' <- checkFuncoes funcoes
  corpoFuncoes' <- checkCorpoFuncoes funcoes [] corpoFuncoes
  variaveisBlocoPrincipal' <- checkVariaveis variaveisBlocoPrincipal
  blocoPrincipal' <- checkBloco funcoes variaveisBlocoPrincipal' Nothing blocoPrincipal
  pure (Prog funcoes' corpoFuncoes' variaveisBlocoPrincipal' blocoPrincipal')

main = do
  putStrLn "Rodando analisador semantico"
  arquivo <- getLine
  conteudo <- readFile arquivo
  let ast = P.genAST conteudo
  let MS (erros_avisos, ast') = checkAST ast
  putStrLn erros_avisos
  print ast'

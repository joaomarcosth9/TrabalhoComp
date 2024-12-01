module Semantic where

import Parser qualified

data M a = MS (String, a) deriving Show

instance Functor M where
  fmap f (MS (s, a)) = MS (s, f a)

instance Applicative M where
  pure a = MS ("", a)
  MS (s1, f) <*> MS (s2, x) = MS (s1 <> s2, f x)   

instance Monad M where
--  return a = MS ("", a)
  MS m >>= f = let (s, a) = m in let MS (s', b) = f a in MS (s++s', b)
 
erro s = MS ("Erro:"++s, ())

adv s = MS ("Advertencia:"++s, ())

--- Implementar aqui o analisador semantico

main = do
    putStrLn "Rodando analisador semantico"
    arquivo <- getLine
    conteudo <- readFile arquivo
    print (Parser.genAST conteudo)


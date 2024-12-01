module TesteMonadaM where

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

-- Função que verifica se um número é positivo
checkPositive :: Int -> M Int
checkPositive n
  | n > 0     = pure n
  | n == 0    = do
      emiteAviso "O numero eh zero."
      pure n
  | otherwise = do
      emiteErro "O numero eh negativo."
      pure n

      -- Função que soma dois resultados de checkPositive
sumCheckPositives :: M Int -> M Int -> M Int
sumCheckPositives m1 m2 = do
  x <- m1
  y <- m2
  pure (x + y)



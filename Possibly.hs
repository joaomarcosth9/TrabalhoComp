-- Definição do tipo
data Possibly a = Some a | None
  deriving (Show, Eq)

-- Implementando Functor para que você possa usar fmap
instance Functor Possibly where
  fmap _ None     = None
  fmap f (Some x) = Some (f x)

-- Implementando Applicative para trabalhar com <*> e pure
instance Applicative Possibly where
  pure = Some
  None <*> _ = None
  (Some f) <*> something = fmap f something

-- Implementando Monad para permitir o uso de >>= (bind)
instance Monad Possibly where
  return = Some  -- `return` é sinônimo de `pure`
  None >>= _ = None
  (Some x) >>= f = f x

-- Algumas funções utilitárias para testar
safeDiv :: Int -> Int -> Possibly Int
safeDiv _ 0 = None
safeDiv x y = Some (x `div` y)

addOne :: Int -> Possibly Int
addOne x = Some (x + 1)

-- Exemplo de uso
example :: Possibly Int
example = do
  result <- safeDiv 10 0
  addOne result


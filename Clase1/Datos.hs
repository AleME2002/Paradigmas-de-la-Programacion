module Datos (MaybeInt(..), sumar) where

data Dir = Norte | Sur | Este | Oeste
    deriving  (Eq, Show)

opuesta :: Dir -> Dir
opuesta Norte = Sur
opuesta Este = Oeste
opuesta Sur = Norte
opuesta Oeste = Este

-- >>> opuesta Norte
-- C:\Users\Alejo\AppData\Local\Temp\ext9835: withFile: resource busy (file is locked)

data MaybeInt = UnInt Int | Nada 
-- Nada :: MaybeInt
-- UnInt :: Int -> MaybeInt
-- UnInt 4 :: MaybeInt

sumar :: MaybeInt -> Int -> MaybeInt
sumar Nada _ = Nada
sumar (UnInt i) k = UnInt (i + k)

data Par = ParC Int Int
  deriving (Show)

-- ParC :: Int -> Int -> Par

sumaPar :: Par -> Par -> Par
sumaPar (ParC a b) (ParC c d) = ParC (a + c) (b + d)

-- >>> sumaPar (ParC 1 2) (ParC 10 20)
-- ParC 11 22

data Maybe a = Just a | Nothing
  deriving (Show)

-- Just :: a -> Maybe a

data Par2 a = Par2C a a

data Par3 a b = Par3C a b
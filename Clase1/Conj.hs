module Conj (Conj, vacio, insertar, pertenece) where

data Conj a = CConj [a]
  deriving (Show)

vacio :: Conj a
vacio = CConj []

pertenece :: (Eq a) => a -> Conj a -> Bool
pertenece e (CConj xs) = e `elem` xs

-- insertar :: (Eq a) => a -> Conj a -> Conj a
-- insertar e (CConj xs)
--   | e `elem` xs = (CConj xs)
--   | otherwise = CConj (e : xs)

insertar :: (Eq a) => a -> Conj a -> Conj a
insertar e c@(CConj xs)
  | e `pertenece` c = c
  | otherwise = CConj (e : xs)

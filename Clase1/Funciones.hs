import Datos (MaybeInt(..), sumar)

factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)
factorial n | n < 0 = error "Tiene que ser >= 0"

-- >>> factorial 5
-- 120

sumaN :: Int -> [Int] -> [Int]
sumaN _ [] = []
sumaN k (x:xs) = k + x : (sumaN k xs)

-- >>> sumaN 3 [1,2,3]
-- [4,5,6]

sumaN' :: Int -> [Int] -> [Int]
sumaN' k = map (+k)

-- >>> sumaN' 3 [1,2,3]
-- [4,5,6]

aparece :: (Eq t) => t -> [t] -> Bool
aparece _ [] = False
aparece c (x:xs) = c == x || aparece c xs

-- >>> aparece 3 [3,4,5]
-- True

ordenar :: [Int] -> [Int] 
ordenar [] = []
ordenar (x:xs) = minimo x (x:xs) : ordenar (sacarMinimo (minimo x (x:xs)) (x:xs))

minimo :: Int -> [Int] -> Int
minimo n [] = n
minimo n (x:xs) | n <= x = minimo n xs
minimo n (x:xs) | n > x = minimo x xs

sacarMinimo :: Int -> [Int] -> [Int]
sacarMinimo _ [] = []
sacarMinimo n (x:xs) | n == x = xs
sacarMinimo n (x:xs) | n /= x = x : sacarMinimo n xs

-- >>> ordenar [7,3,2,5,1,8,0,7]
-- [0,1,2,3,5,7,7,8]

elUltimoIndice :: (Eq a) => a -> [a] -> Int
elUltimoIndice e (x : xs)
  | elem e xs = 1 + elUltimoIndice e xs
  | e == x = 0

-- elUltimoIndiceSino :: (Eq a) => Int -> a -> [a] -> Int
-- elUltimoIndiceSino r _ [] = r
-- elUltimoIndiceSino r e (x:xs)
--     | e == x && not (elem e xs) = 0

fromJust :: Maybe a -> a
fromJust (Just v) = v

func :: a -> (a -> b) -> b
func x f = f x

elPrimerIndice :: Int -> [Int] -> Int
elPrimerIndice n (x:xs) 
  | x == n = 0
  | otherwise = elPrimerIndice n xs + 1

-- >>> elPrimerIndice 4 [3,5,6,4]
-- 3



elUltimoIndice' :: (Eq a) => a -> [a] -> Maybe Int
elUltimoIndice' _ [] = Nothing
elUltimoIndice' e (x : xs) =
  case elUltimoIndice' e xs of
    Nothing ->
      if e == x
        then Just 0
        else Nothing
    Just r ->
      Just (r + 1)

elUltimoIndice'' :: Int -> [Int] -> Int
elUltimoIndice'' n l = length l - 1 - elPrimerIndice n (reverse l)

-- >>> elUltimoIndice 1 [1,2,3,4,1,3,4,1,3]
-- 7



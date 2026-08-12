import Prelude hiding (Maybe(..), Either(..))
import System.Win32 (xBUTTON1)
import Distribution.Simple.Utils (findFileCwdWithExtension)

--------------------Ejercicio 2--------------------
--a
valorAbsoluto :: Float -> Float
valorAbsoluto n 
    | n >= 0 = n
    | otherwise = -n


--b
bisiesto :: Int -> Bool
bisiesto n
    | mod n 4 == 0 = True
    | otherwise = False


--c
factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)
factorial n | n < 0 = error "Tiene que ser >= 0"


--d
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = cantDivisoresPrimosAux n 2

cantDivisoresPrimosAux :: Int -> Int -> Int
cantDivisoresPrimosAux a b
  | b > a = 0  
  | mod a b == 0 && esPrimo b 2 = 1 + cantDivisoresPrimosAux a (b + 1)
  | otherwise = cantDivisoresPrimosAux a (b + 1)

-- >>> cantDivisoresPrimos 10
-- 2

esPrimo :: Int -> Int -> Bool
esPrimo n d
  | n <= 1       = False
  | d * d > n    = True
  | mod n d == 0 = False
  | otherwise    = esPrimo n (d + 1)

-- >>> esPrimo 2 2
-- True


--------------------Ejercicio 3--------------------
data Maybe a = Just a | Nothing
  deriving (Show)

data Either a b = Left a | Right b
  deriving (Show)


--a
inverso :: Float -> Maybe Float
inverso n | n == 0 = Nothing
inverso n = Just(1 / n)

-- >>> inverso 5
-- Just 0.2


--b
aEntero :: Either Int Bool -> Int
aEntero (Right False) = 0
aEntero (Right True) = 1
aEntero (Left x) = x

-- >>> aEntero (Right False)
-- 0
-- >>> aEntero (Right True)
-- 1
-- >>> aEntero (Left 67)
-- 67


--------------------Ejercicio 4--------------------
--a
limpiar :: String -> String -> String
limpiar _ [] = []
limpiar [] b = b
limpiar  (x:xs) (y:ys) = limpiar xs (limpiarAux x (y:ys))

limpiarAux :: Char -> String -> String
limpiarAux _ [] = []
limpiarAux l (x:xs) 
    | x == l = limpiarAux l xs
    | otherwise = x : limpiarAux l xs

-- >>> limpiarAux 'e' "elefante"
-- "lfant"
-- >>> limpiar "helado" "elefante"
-- "fnt"


--b
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio p = difPromedioAux (promedio p) p

-- >>> difPromedio [2,3,4]
-- [-1.0,0.0,1.0]


promedio :: [Float] -> Float
promedio [] = 0
promedio p = sum p / fromIntegral (length p)

-- >>> promedio [2,3,4]
-- 3.0

difPromedioAux :: Float -> [Float] -> [Float]
difPromedioAux _ [] = []
difPromedioAux n (x:xs) = x - n : difPromedioAux n xs


--c
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)


-- >>> todosIguales [1,1,1,1,1,1,1]
-- True


--------------------Ejercicio 5--------------------
data AB a = Nil | Bin (AB a) a (AB a)
    deriving (Eq,Show)

arbolBoolVacio :: AB Bool
arbolBoolVacio = Nil

arbolBoolEjemplo :: AB Bool
arbolBoolEjemplo = Bin (Bin Nil False Nil) True (Bin Nil True Nil)
--      True
--     /    \
--  False  True

arbolChico :: AB Int
arbolChico = Bin (Bin Nil 2 Nil) 3 (Bin Nil 4 Nil)
--      3
--     / \
--    2   4

arbolGrande :: AB Int
arbolGrande = Bin (Bin (Bin Nil 4 Nil) 3 (Bin Nil 1 Nil)) 2 (Bin Nil 5 (Bin Nil 2 Nil))
--         2
--       /   \
--      3     5
--     / \     \
--    4   1     2

preorder :: AB a -> [a]
preorder Nil = []
preorder (Bin i r d) = (r : preorder i) ++ preorder d


--a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- >>> vacioAB arbolBoolVacio
-- True
-- >>> vacioAB arbolBoolEjemplo
-- False


--b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

-- >>> negacionAB arbolBoolEjemplo
-- Bin (Bin Nil True Nil) False (Bin Nil False Nil)


--c
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = r * productoAB i * productoAB d

-- >>> productoAB arbolChico
-- 24

-- >>> productoAB arbolGrande
-- 240



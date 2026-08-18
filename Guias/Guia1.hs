--------------------Ejercicio 1--------------------

-- max2 :: (Float -> Float) -> Float                     max2 :: (Float -> Float) -> Float
-- normaVectorial :: (Float, Float) -> Float
-- subtract :: Float -> Float -> Float
-- predecesor :: Float -> Float
-- evaluarEnCero :: (Float -> a) -> a
-- dosVeces :: (a -> a) -> a -> a
-- flipAll :: 
-- flipRaro :: 


--------------------Ejercicio 2--------------------

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- curryN no se podria ya que necesitaria una con 4 uplas y mas


--------------------Ejercicio 3--------------------

listaDesde :: Num t => t -> [t]
listaDesde x = x : listaDesde (x+1)

esMultiploDe10 :: Integral a => a -> Bool
esMultiploDe10 n = mod n 10 == 0

takeHastaMultiploDe10 :: Integral a => [a] -> [a]
takeHastaMultiploDe10 [] = []
takeHastaMultiploDe10 (x:xs) = if esMultiploDe10 x then [] else x : takeHastaMultiploDe10 xs

{-Mostrar paso a paso cómo reduce Haskell la expresión takeHastaMultiploDe10 (listaDesde 29)
takeHastaMultiploDe10 (listaDesde 29)
takeHastaMultiploDe10 (29 : listaDesde (29 + 1))
if esMultiploDe10 29 then [] else 29 : takeHastaMultiploDe10 (listaDesde (29 + 1))
29 : takeHastaMultiploDe10 (listaDesde 30)
29 : takeHastaMultiploDe10 (30 : listaDesde (30 + 1))
29 : (if esMultiploDe10 30 then [] else 30 : takeHastaMultiploDe10 (listaDesde (30 + 1)))
29 : [] 
[29]
-}


--------------------Ejercicio 4--------------------






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






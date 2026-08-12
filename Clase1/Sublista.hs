
subsecuencia :: [a] -> [[a]]
subsecuencia [] = [[]]
subsecuencia (x : xs) = 
        subsecuencia xs ++ (agTodas x (subsecuencia xs)) 


agTodas :: a -> [[a]] -> [[a]]
-- agTodas e xss = map (\xs -> e:xs) xss
agTodas e = map (e:) 


--       0      subsecuencia [1,2] ~~~> 
--                  [[], [1], [2], [1,2]]



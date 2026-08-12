module Dict (Dict, vacio) where

import Arbol

data Dict k v = CDict (AB (k, v))

vacio :: Dict k v
vacio = CDict Nil

definir :: (Ord k) => k -> v -> Dict k v -> Dict k v
definir clave valor (CDict ab) = CDict (definirAB clave valor ab)

definirAB :: (Ord k) => k -> v -> AB (k, v) -> AB (k, v)
definirAB clave valor Nil = Bin Nil (clave, valor) Nil
-- definirAB clave valor (Bin i (k, v) d)
--   | clave == k = Bin i (k, valor) d
--   | clave < k = Bin (definirAB clave valor i) (k, v) d
--   | clave > k = Bin i (k, v) (definirAB clave valor d)
definirAB clave valor (Bin i (k, v) d) =
  case compare clave k of
    EQ -> Bin i (k, valor) d
    LT -> Bin (definirAB clave valor i) (k, v) d
    GT -> Bin i (k, v) (definirAB clave valor d)

-- buscar :: Ord k => k -> Dict k v -> Maybe v

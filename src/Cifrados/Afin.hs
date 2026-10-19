module Cifrados.Afin where

import Cifrados.Cesar
import Cifrados.Decimado
import Cifrados.Tipos
import Cifrados.Utilidades

type ClaveAfin = (Int, Int)

-- |
-- Hacemos el cifrado Afín combinando dos cifrados. Primero aplicamos el cifrado decimado usando 'Cifrados.Decimado.cifrarDecimado' con el factor @a@ y luego usamos 'Cifrados.Cesar.cifrarCesar' para sumarle el desplazamiento @b@. Esto representa la fórmula \(E_{a,b}(x) \equiv a \cdot x + b \pmod{m}\).
cifrarAfin :: Alfabeto -> ClaveAfin -> String -> String
cifrarAfin sigma (a, b) cadena =
  cifrarCesar sigma b (cifrarDecimado sigma a cadena)

-- |
-- Para el descifrado Afín, hacemos las operaciones al revés. Primero usamos 'Cifrados.Cesar.descifrarCesar' con la llave @b@ para deshacer la suma. Después usamos 'Cifrados.Utilidades.inversoModular' para revisar si el coeficiente @a@ tiene inverso modular (si es primo relativo con la longitud del alfabeto). Si lo tiene, usamos 'Cifrados.Utilidades.transformarLetras' para multiplicar el resultado por el inverso de @a@. Si no, regresamos un mensaje de error.
descifrarAfin :: Alfabeto -> ClaveAfin -> String -> String
descifrarAfin sigma (a, b) cadena = case inversoModular a (length sigma) of
  Just inv ->
    let noCesar = descifrarCesar sigma b cadena
     in transformarLetras sigma (* inv) noCesar
  Nothing -> "Error: La clave a=" ++ show a ++ " no tiene inverso modular."

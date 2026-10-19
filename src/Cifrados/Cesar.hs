module Cifrados.Cesar where

import Cifrados.Tipos
import Cifrados.Utilidades

type ClaveCesar = Int

{-|
Utilizamos la función 'Cifrados.Utilidades.transformarLetras' de las utilidades para aplicar a cada uno de los caracteres de la cadena a cifrar la función que suma la clave de cifrado para hacer el "recorrido" que se hace al cifrar con César.
-}

cifrarCesar :: Alfabeto -> ClaveCesar -> String -> String
cifrarCesar sigma k cadena = transformarLetras sigma (+ k) cadena

{-|
Hacemos el mismo procedimiento pero restando la clave (usando la función 'Prelude.subtract') para aplicar el retroceso de @k@ módulo longitud del alfabeto y así hacer el recorrido inverso sabiendo la clave @k@.
-}
descifrarCesar :: Alfabeto -> ClaveCesar -> String -> String
descifrarCesar sigma k cadena = transformarLetras sigma (subtract k) cadena

module Cifrados.Decimado where

import Cifrados.Tipos
import Cifrados.Utilidades

type ClaveDecimado = Int

{-|
Función para hacer el cifrado decimado. Para ello, utilizamos la función de las utilidades 'Cifrados.Utilidades.transformarLetras' para multiplicar el valor numérico de cada carácter por la llave @k@ y aplicar el módulo al resultado.
-}
cifrarDecimado :: Alfabeto -> ClaveDecimado -> String -> String
cifrarDecimado sigma k cadena = transformarLetras sigma (* k) cadena

{-|
Función para hacer el descifrado decimado. Para ello, necesitamos el inverso modular de la clave @k@, con lo que utilizamos la función de utilidad 'Cifrados.Utilidades.inversoModular' para obtener el inverso y multiplicar lo obtenido con la representación numérica de cada carácter en la cadena.
Utilizamos la expresión @case@ porque solo se puede descifrar si es que existe el inverso modular, y esto ocurre siempre que la llave @k@ y la longitud del alfabeto son primos relativos.
Si es que existe, hacemos el procedimiento que se describió.
-}
descifrarDecimado :: Alfabeto -> ClaveDecimado -> String -> String
descifrarDecimado sigma k cadena = case inversoModular k (length sigma) of
  Just inv -> transformarLetras sigma (* inv) cadena
  Nothing -> "Error: La clave " ++ show k ++ " no tiene inverso modular."

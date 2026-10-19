module Cifrados.Vigenere where

import Cifrados.Tipos
import Cifrados.Utilidades

-- Dado que en este caso la clave de entrada es un texto, definimos la llave como una cadena.
type ClaveVigenere = String

-- |
-- Función auxiliar para el Cifrado Vigenère. Para evitar que la clave se desacomode cuando hay espacios o símbolos, recorremos el texto a mano. Revisamos si cada carácter está en el alfabeto usando 'Cifrados.Utilidades.indiceLetra'. Si está, tomamos el desplazamiento de la clave y sumamos o restamos dependiendo si queremos cifrar o descifrar. Usamos recursión para ir avanzando en la clave (@ks@) o repetir el ciclo (@cicloClave@) solo cuando las letras son válidas.
procesarVigenere :: Alfabeto -> ClaveVigenere -> String -> Bool -> String
procesarVigenere sigma clave cadena esCifrar = iterar cadena (cycle clavesNumericas)
  where
    -- Convertimos la palabra clave a una lista de numeros.
    clavesNumericas = map (\c -> case indiceLetra sigma c of Just i -> i; Nothing -> 0) clave
    -- La funcion iterar recorre el texto y la lista de claves simultaneamente
    iterar [] _ = []
    iterar (letra : restoTexto) ks@(k : restoClave) =
      case indiceLetra sigma letra of
        -- Si el caracter pertenece al alfabeto, aplicamos la suma/resta y AVANZAMOS la clave.
        Just i ->
          let nuevoIndice = if esCifrar then i + k else i - k
           in letraDeNumero sigma nuevoIndice : iterar restoTexto restoClave
        -- Si es un espacio/simbolo, lo dejamos igual y pasamos 'ks' sin consumir 'k')
        Nothing -> letra : iterar restoTexto ks

-- |
-- Llamamos a la función 'Cifrados.Vigenere.procesarVigenere' pasándole un @True@ para indicar que queremos sumar el desplazamiento de la clave.
cifrarVigenere :: Alfabeto -> ClaveVigenere -> String -> String
cifrarVigenere sigma clave cadena = procesarVigenere sigma clave cadena True

-- |
-- Llamamos a la función 'Cifrados.Vigenere.procesarVigenere' pasándole un @False@ para indicar que queremos restar el desplazamiento de la clave.
descifrarVigenere :: Alfabeto -> ClaveVigenere -> String -> String
descifrarVigenere sigma clave cadena = procesarVigenere sigma clave cadena False

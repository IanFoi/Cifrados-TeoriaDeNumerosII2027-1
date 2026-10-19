module Cifrados.Hill where

import Cifrados.Tipos
import Cifrados.Utilidades
import Data.Maybe (fromJust, isJust)

type ClaveHill = [[Int]]

-- |
-- Hace el cifrado de Hill multiplicando matrices. Primero revisamos si se puede invertir la matriz usando 'Cifrados.Utilidades.determinante' y 'Cifrados.Utilidades.algEuclides', verificando que el determinante y el tamaño del alfabeto sean primos relativos. Limpiamos el texto con 'Prelude.filter' para quitar caracteres inválidos (los que no pasan 'Cifrados.Utilidades.indiceLetra'). Después partimos el texto en bloques del tamaño de la matriz con 'Cifrados.Utilidades.romperCadena', multiplicamos la llave por cada bloque usando 'Cifrados.Utilidades.multiplicarMV' y regresamos los números a letras con 'Cifrados.Utilidades.letraDeNumero'.
cifrarHill :: Alfabeto -> ClaveHill -> String -> String
cifrarHill sigma matriz cadena =
  let d = determinante matriz
      m = length sigma
      (g, _, _) = algEuclides (abs d) m
   in if g /= 1
        then "Error: Determinante " ++ show d ++ " no es coprimo con " ++ show m
        else
          let cadenaLimpia = filter (\c -> isJust (indiceLetra sigma c)) cadena
              n = length matriz
              bloques = romperCadena n cadenaLimpia

              cifrarBloque bloque =
                let v = map (\c -> fromJust (indiceLetra sigma c)) bloque
                    vCifrado = multiplicarMV matriz v
                 in map (\num -> letraDeNumero sigma num) vCifrado
           in concatMap cifrarBloque bloques

-- |
-- Descifra un texto de Hill calculando la matriz inversa modular. Usamos la función 'Cifrados.Utilidades.inversaMatrizMod' que internamente saca la adjunta y multiplica por el inverso del determinante módulo la longitud del alfabeto. Con un @case@ checamos si existe la inversa; si sí, volvemos a llamar a la función 'Cifrados.Hill.cifrarHill' pasándole la @matrizInversa@ en lugar de la original, ya que descifrar es como volver a cifrar pero con la matriz inversa.
descifrarHill :: Alfabeto -> ClaveHill -> String -> String
descifrarHill sigma matriz cadena =
  case inversaMatrizMod matriz (length sigma) of
    Nothing -> "Error: La matriz no tiene inversa modular en base " ++ show (length sigma)
    Just matrizInversa -> cifrarHill sigma matrizInversa cadena

module Cifrados.AlbertiLetras (cifrarAlbertiLetras, descifrarAlbertiLetras) where

import Cifrados.Tipos
import Data.Char (isUpper, toLower, toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type ClaveAlbertiLetras = (Char, Char)

discoExterior :: String
discoExterior = "ABCDEFGILMNOPQRSTVXZ1234"

discoInterior :: String
discoInterior = "acegklnprtvz&xysomqihfdb"

normalizarExterior :: Char -> String
normalizarExterior c =
  let u = toUpper c
   in if u `elem` discoExterior
        then [u]
        else case u of
          'J' -> "I"
          'U' -> "V"
          'W' -> "V"
          'Y' -> "I"
          'H' -> "G"
          'K' -> "C"
          'N' -> "N"
          _ -> ""

alinearDiscos :: Char -> Char -> [(Char, Char)]
alinearDiscos exterior interior =
  let idxOut = fromJust (elemIndex exterior discoExterior)
      idxIn = fromJust (elemIndex interior discoInterior)
      desplazamiento = (idxIn - idxOut) `mod` 24
      innerRotado = drop desplazamiento discoInterior ++ take desplazamiento discoInterior
   in zip discoExterior innerRotado

-- |
-- Hace el cifrado usando los Discos de Alberti con puras letras. Empleamos 'normalizarExterior' y 'Data.Char.toUpper' para limpiar el texto de entrada y cambiar letras equivalentes (como la J por la I). Recorremos el texto usando recursión para llevar la cuenta de la posición del disco. Con un 'Data.Char.isUpper' verificamos si la letra es mayúscula; si lo es, usamos 'Cifrados.AlbertiLetras.alinearDiscos' (que usa 'Data.List.elemIndex' para ver cuánto rotar el disco interior) y actualizamos la posición, imprimiendo la mayúscula sin cifrar. Si es minúscula, la buscamos y la ciframos normalmente.
cifrarAlbertiLetras :: Alfabeto -> ClaveAlbertiLetras -> String -> String
cifrarAlbertiLetras _ (outK, inK) cadena =
  let cifrar _ [] = ""
      cifrar idxOut (c : cs)
        | isUpper c =
            let norm = normalizarExterior c
             in if null norm
                  then cifrar idxOut cs
                  else c : cifrar (head norm) cs
        | otherwise =
            let norm = normalizarExterior c
             in if null norm
                  then cifrar idxOut cs
                  else
                    let u = head norm
                        alineacion = alinearDiscos idxOut inK
                        coincidencias = filter (\(o, _) -> o == u) alineacion
                     in if null coincidencias
                          then '?' : cifrar idxOut cs
                          else snd (head coincidencias) : cifrar idxOut cs
   in cifrar outK cadena

-- |
-- Descifra un texto de Alberti en letras. Al igual que el cifrado, usamos un condicional 'Data.Char.isUpper' al recorrer la lista recursivamente. Si leemos una mayúscula, actualizamos el índice externo \(@idxOut@\) y pasamos al siguiente carácter de la cadena (\(@cs@\)) ignorando la mayúscula en el resultado. Si es una minúscula, llamamos a 'Cifrados.AlbertiLetras.alinearDiscos' y usamos 'Prelude.filter' para buscar qué letra del exterior le toca. Si por algo no la encuentra, ponemos un '?' para que no falle.
descifrarAlbertiLetras :: Alfabeto -> ClaveAlbertiLetras -> String -> String
descifrarAlbertiLetras _ (outK, inK) cadena =
  let descifrar _ [] = ""
      descifrar idxOut (c : cs)
        | isUpper c =
            let norm = normalizarExterior c
             in if null norm
                  then descifrar idxOut cs
                  else descifrar (head norm) cs
        | otherwise =
            let alineacion = alinearDiscos idxOut inK
                coincidencias = filter (\(_, i) -> i == c) alineacion
             in if null coincidencias
                  then '?' : descifrar idxOut cs
                  else fst (head coincidencias) : descifrar idxOut cs
   in descifrar outK cadena

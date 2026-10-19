module Cifrados.AlbertiNumeros (cifrarAlbertiNumeros, descifrarAlbertiNumeros) where

import Cifrados.Tipos
import Data.Char (toLower, toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type ClaveAlbertiNumeros = (Char, Char)

discoExterior :: String
discoExterior = "ABCDEFGILMNOPQRSTVXZ1234"

discoInterior :: String
discoInterior = "acegklnprtvz&xysomqihfdb"

normalizarExteriorNumeros :: Char -> String
normalizarExteriorNumeros c =
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
          '\209' -> "N"
          _ -> ""

alinearDiscosNumeros :: Char -> Char -> [(Char, Char)]
alinearDiscosNumeros exterior interior =
  let idxOut = fromJust (elemIndex exterior discoExterior)
      idxIn = fromJust (elemIndex interior discoInterior)
      desplazamiento = (idxIn - idxOut) `mod` 24
      innerRotado = drop desplazamiento discoInterior ++ take desplazamiento discoInterior
   in zip discoExterior innerRotado

-- |
-- Cifra un texto de Alberti pero ahora con los números disparando el cambio de giro. Usamos 'Cifrados.AlbertiNumeros.normalizarExteriorNumeros' para arreglar las letras. Al ir recorriendo la lista, usamos 'Prelude.lookup' en el resultado de 'Cifrados.AlbertiNumeros.alinearDiscosNumeros' para ver en qué se cifra la letra. Luego usamos 'Prelude.elem' para checar si el símbolo original era @"1234"@; si sí lo era, la función cambia la llave interior \(@innerKey@\) usando ese carácter cifrado como el nuevo pivote de rotación para la recursión.
cifrarAlbertiNumeros :: Alfabeto -> ClaveAlbertiNumeros -> String -> String
cifrarAlbertiNumeros _ (outK, inK) cadena =
  let texto = concatMap normalizarExteriorNumeros cadena

      cifrarNum _ _ [] = ""
      cifrarNum outerKey innerKey (x : xs) =
        let alineacion = alinearDiscosNumeros outerKey innerKey
            cifrado = fromJust (lookup x alineacion)
         in if x `elem` "1234"
              then cifrado : cifrarNum outerKey cifrado xs
              else cifrado : cifrarNum outerKey innerKey xs
   in cifrarNum outK inK texto

-- |
-- Descifra Alberti usando números como pivotes. Como los números se cifraron, tenemos que buscarlos al revés. Filtramos el disco con 'Prelude.filter' y sacamos el primer carácter que coincide usando 'Prelude.fst' y 'Prelude.head'. Después, usamos 'Prelude.elem' para preguntar si esa letra original que acabamos de sacar está en @"1234"@. Si resulta que sí era uno de los números indicadores, lo usamos para hacer que la recursión gire el disco \(@innerKey@\) y no lo escribimos en el texto claro final.
descifrarAlbertiNumeros :: Alfabeto -> ClaveAlbertiNumeros -> String -> String
descifrarAlbertiNumeros _ (outK, inK) cadena =
  let descifrar _ _ [] = ""
      descifrar outerKey innerKey (c : cs) =
        let alineacion = alinearDiscosNumeros outerKey innerKey
            coincidencias = filter (\(_, i) -> i == c) alineacion
         in if null coincidencias
              then '?' : descifrar outerKey innerKey cs
              else
                let outC = fst (head coincidencias)
                 in if outC `elem` "1234"
                      then descifrar outerKey c cs
                      else outC : descifrar outerKey innerKey cs
   in descifrar outK inK cadena

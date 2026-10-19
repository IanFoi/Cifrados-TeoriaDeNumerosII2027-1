module Cifrados.RSA where

import Cifrados.Tipos
import Cifrados.Utilidades

type ClavePublicaRSA = (Integer, Integer) -- (e, n)

type ClavePrivadaRSA = (Integer, Integer) -- (d, n)

-- |
-- Cifra un texto con RSA. Convertimos cada letra de la cadena en un número de dos dígitos usando 'Cifrados.Utilidades.letraANumeroRSA'. Con la función 'Cifrados.Utilidades.tamanoBloque' calculamos @k@, que es cuántos dígitos podemos juntar sin pasarnos de @n@. Si el texto no cabe exacto en bloques de tamaño @k@, le rellenamos con espacios representados por ("99") al final. Cortamos el texto con 'Cifrados.Utilidades.partirEnBloques' y elevamos cada bloque usando 'Cifrados.Utilidades.expMod' para calcular \(M^e \pmod{n}\). Al final regresamos los números separados por espacios con 'Prelude.unwords'.
cifrarRSA :: Alfabeto -> ClavePublicaRSA -> String -> String
cifrarRSA sigma (n, e) cadena =
  let numStr = concatMap (letraANumeroRSA sigma) cadena
      k = tamanoBloque n

      -- Rellenar con espacios (99) si el ultimo bloque queda incompleto
      sobrante = length numStr `mod` k
      numStrPadded = if sobrante == 0 then numStr else numStr ++ concat (replicate ((k - sobrante) `div` 2) "99")

      bloquesText = partirEnBloques k numStrPadded
      bloquesNum = map (\b -> read b :: Integer) bloquesText
      bloquesCifrados = map (\m -> expMod m e n) bloquesNum
   in unwords (map show bloquesCifrados)

-- |
-- Descifra RSA. Recibe los números separados por espacios, los extrae con 'Prelude.words' y los pasa a enteros con 'Prelude.reads'. Los elevamos al exponente privado @d@ usando 'Cifrados.Utilidades.expMod' para sacar \(C^d \pmod{n}\). Después de eso, recuperamos los ceros que se hayan perdido a la izquierda con un @replicate@ para que todos los bloques midan exactamente @k@. Por último, volvemos a separar en parejitas de 2 dígitos y las convertimos a letras con 'Cifrados.Utilidades.numeroALetraRSA'.
descifrarRSA :: Alfabeto -> ClavePrivadaRSA -> String -> String
descifrarRSA sigma (n, d) cadena =
  let wordsStr = words cadena
      bloquesCifrados = map (\s -> case reads s of [(val, "")] -> val; _ -> 0) wordsStr
      bloquesDescifrados = map (\c -> expMod c d n) bloquesCifrados
      k = tamanoBloque n

      -- Recuperar ceros a la izquierda que se pudieron perder al volverse entero
      formatearBloque b =
        let s = show b
         in replicate (k - length s) '0' ++ s

      numStr = concatMap formatearBloque bloquesDescifrados
      letras = partirEnBloques 2 numStr
   in map (numeroALetraRSA sigma) letras

module Cifrados.ElGamal where

import Cifrados.Tipos
import Cifrados.Utilidades
import Data.List (intersperse)

type ClavePublicaElGamal = (Integer, Integer, Integer) -- (p, g, a)

type ClavePrivadaElGamal = (Integer, Integer) -- (p, e)

-- |
-- Hace el cifrado de ElGamal por bloques. Igual que en RSA, usamos 'Cifrados.Utilidades.letraANumeroRSA' y 'Cifrados.Utilidades.tamanoBloque' para partir el texto en bloques de números. Después calculamos el parámetro \(\gamma = g^k \pmod{p}\) y el factor \(\alpha = a^k \pmod{p}\) usando la función 'Cifrados.Utilidades.expMod'. Multiplicamos cada bloque por \(\alpha\) y lo devolvemos junto a \(\gamma\) armando una lista de tuplas en formato de texto usando @unwords [show (gam, bet)]@.
cifrarElGamal :: Alfabeto -> ClavePublicaElGamal -> Integer -> String -> String
cifrarElGamal sigma (p, g, a) k cadena =
  let numStr = concatMap (letraANumeroRSA sigma) cadena
      kTexto = tamanoBloque p
      sobrante = length numStr `mod` kTexto
      numStrPadded = if sobrante == 0 then numStr else numStr ++ concat (replicate ((kTexto - sobrante) `div` 2) "99")
      bloquesText = partirEnBloques kTexto numStrPadded
      bloquesNum = map (\b -> read b :: Integer) bloquesText

      -- Matematicas ElGamal
      gamma = expMod g k p
      alfa = expMod a k p
      -- Cada tupla cifrada es (gamma, b_i * alfa mod p)
      bloquesCifrados = map (\b -> (gamma, (b * alfa) `mod` p)) bloquesNum
   in unwords [show (gam, bet) | (gam, bet) <- bloquesCifrados]

-- |
-- Descifra una lista de tuplas hecha con ElGamal. Leemos la cadena de texto como si fuera una lista de Haskell poniéndole corchetes, comas con 'Data.List.intersperse' y usando 'Prelude.reads'. Sacamos \(\gamma\) del primer elemento de la lista y calculamos el inverso modular usando con 'Cifrados.Utilidades.expMod' y el exponente \(p - 1 - e\). Multiplicamos esto por la segunda parte de la tupla (el número cifrado) y le volvemos a poner los ceros de la izquierda. Para terminar, llamamos a 'Cifrados.Utilidades.numeroALetraRSA'.
descifrarElGamal :: Alfabeto -> ClavePrivadaElGamal -> String -> String
descifrarElGamal sigma (p, e) cadena =
  -- Leemos la lista de tuplas emulando el formato de Python
  -- Convertimos "(8,62) (8,23)" a "[(8,62),(8,23)]"
  let limpiar c = if c == ')' then ')' else c
      -- words separa por espacios, luego unimos con comas y encerramos en corchetes
      cadenaLista = "[" ++ concat (intersperse "," (words cadena)) ++ "]"
      parsedLista = reads cadenaLista :: [([(Integer, Integer)], String)]
      lista = case parsedLista of
        [(res, "")] -> res
        _ -> [(1, 0)] -- Valores por defecto seguros si hay error de parseo
      invE = (p - 1) - e
      -- gamma es el primer elemento de la primera tupla
      gamma = if null lista then 1 else fst (head lista)
      gammaInv = expMod gamma invE p

      bloquesDescifrados = map (\(_, b) -> (gammaInv * b) `mod` p) lista

      kTexto = tamanoBloque p
      formatearBloque b =
        let s = show b
         in replicate (kTexto - length s) '0' ++ s

      numStr = concatMap formatearBloque bloquesDescifrados
      letras = partirEnBloques 2 numStr
   in map (numeroALetraRSA sigma) letras

module Cifrados.Utilidades where

import Cifrados.Tipos
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

{-|
Busca una letra del  alfabeto y devuelve su posicion en el alfabeto, como puede ser un simbolo que no pertenezca al alfabeto devuelve Maybe Int, devolviedo Just posicion la posicion de la letra o Nothing.
Lee la lista del alfabeto y usa elemIndex para obtener el indice.
 -}
indiceLetra :: Alfabeto -> Char -> Maybe Int
indiceLetra sigma c = elemIndex c sigma

{-|
 Dado un numero, devuelve una letra correspondiente a la posicion en el alfabeto, como podemos recibir un numero mayor a la cantidad de letras en el alfabeto, trabajamos con modulos para hacer que el alfabeto funcione de manera circular como con los circulos que usabamos en cifrado.
Utiliza el operador !! para obtener el elemento que se encuentra en la posicion i en modulo m con m la longitud del alfabeto(obtenida por length).
-}
letraDeNumero :: Alfabeto -> Int -> Char
letraDeNumero sigma n = sigma !! (n `mod` length sigma)

{-|
Recibe el alfabeto, una formula y una cadena de texto. Aplica la formula a todos los caracteres que pertenecen al alfabeto para aplicar la funcion a toda la cadena sin necesidad de aplicarlo en el codigo principal logrando que el codigo principal sea mucho mas limpio, directo y facil de leer.
Los caracteres que no pertenecen al alfabeto (como espacios) se dejan intactos.
-}
transformarLetras :: Alfabeto -> (Int -> Int) -> String -> String
transformarLetras sigma f cadena = map procesar cadena
  where
    procesar c = case indiceLetra sigma c of
      Just i -> letraDeNumero sigma (f i)
      Nothing -> c

{-|
 Funcion para encontrar el inverso modular a partir de resolver la congruencia ax \=/ 1(mod m).
 Recibe el numero alpha, el modulo y devuelve el numero x correspondiente a alpha-1.
Como el inverso modular solo existe si alpha y m son coprimos (a,m) = 1, devuelven Maybe Int pues no podemos asegurar que exista el inverso.
Finalmente, la funcion utiliza la implementacion del algoritmo de Euclides.
 -}
inversoModular :: (Integral a) => a -> a -> Maybe a
inversoModular a m
  | g == 1 = Just (x `mod` m)
  | otherwise = Nothing
  where
    (g, x, _) = algEuclides a m

{-|
Implementacion recursiva del algoritmo de Euclides.
Devuelve una tripla (g, x, y) tal que a*x + b*y = g = mcd(a, b)
-}

algEuclides :: (Integral a) => a -> a -> (a, a, a)
algEuclides 0 b = (b, 0, 1)
algEuclides a b =
  let (g, s, t) = algEuclides (b `mod` a) a
   in (g, t - (b `div` a) * s, s)

{-|
Funcion para multiplicar una matriz NxN por un vector de tamano N.
-}
multiplicarMV :: [[Int]] -> [Int] -> [Int]
multiplicarMV matriz vector = map (\fila -> sum (zipWith (*) fila vector)) matriz

{-|
Funcion para romper la cadena a cifrar en el cifrado Hill en pedazos proporcionales al tamano de la matriz, si el ultimo bloque queda incompleto se repite la ultima letra
-}
romperCadena :: Int -> [a] -> [[a]]
romperCadena _ [] = []
romperCadena n lista
  | length lista < n = [lista ++ replicate (n - length lista) (last lista)]
  | otherwise = take n lista : romperCadena n (drop n lista)

{-|
Funcion auxiliar que elimina un elemento de una lista dado su indice (empezando en 0).
La usamos para eliminar la columna correspondiente al crear la submatriz.
-}
removerIndice :: Int -> [a] -> [a]
removerIndice i xs = take i xs ++ drop (i + 1) xs

{-|
Funcion para encontrar el determinante de una matriz general NxN usando la Expansion de cofactores.
Usamos esta informacion para saber que la matriz tiene inversa, pues necesitamos que el maximo comun divisor entre el determinante y la longitud del alfabeto sea 1 para que podamos descifrar.
-}
determinante :: [[Int]] -> Int
determinante [[x]] = x -- Caso base: matriz 1x1
determinante (fila1 : restoFilas) =
  let calcularCofactor (j, elemento) =
        let subMatriz = map (removerIndice j) restoFilas
            signo = if even j then 1 else -1
         in signo * elemento * determinante subMatriz
   in sum (map calcularCofactor (zip [0 ..] fila1))

{-|
Calcula la matriz adjunta (transpuesta de la matriz de cofactores).
Necesaria para encontrar la inversa de una matriz general.
-}
adjunta :: [[Int]] -> [[Int]]
adjunta matriz =
  let n = length matriz
      cofactor i j =
        let subMatriz = map (removerIndice j) (removerIndice i matriz)
            signo = if even (i + j) then 1 else -1
         in signo * determinante subMatriz
   in [[cofactor j i | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

{-|
Calcula la matriz inversa en aritmetica modular siguiendo la formula A^-1 = det(A)^-1 * Adj(A) (mod m)
-}
inversaMatrizMod :: [[Int]] -> Int -> Maybe [[Int]]
inversaMatrizMod matriz m = do
  let d = determinante matriz
  invD <- inversoModular d m
  let adj = adjunta matriz
  return [[(elemento * invD) `mod` m | elemento <- fila] | fila <- adj]

{-|
Funcion recursiva para hacer la exponenciacion modular rapida b^e mod m.
Si bien Haskell ya tiene una funcion similar powMod para hacer esto, vale la pena el implementarla como ejercicio.
-}
expMod :: Integer -> Integer -> Integer -> Integer
expMod _ 0 _ = 1
expMod base exponente m
  | even exponente = (mitad * mitad) `mod` m
  | otherwise = (base * mitad * mitad) `mod` m
  where
    mitad = expMod base (exponente `div` 2) m

{-|
Funcion para convertir una letra en un codigo numerico RSA, funciona similar a la funcion que ya implementamos pero agrega una codificacion base para el espacio ' ' ademas de que ningun bloque debe de iniciar con cero, por lo que le sumamos 11 para que el primer elemento del alfabeto, generalmente a, tenga esta codificacion.
Acepta cualquier alfabeto proporcionado por el usuario dando una codificacion a partir de su posicion en la lista.
Usamos una cadena en vez de un entero para poder agrupar como se hace en RSA.
-}
letraANumeroRSA :: Alfabeto -> Char -> String
letraANumeroRSA _ ' ' = "99"
letraANumeroRSA sigma c =
  case indiceLetra sigma c of
    Just i -> show (i + 11)
    Nothing -> "99"

{-|
Funcion para convertir una cadena de dos digitos y devuelve su representacion como letra del alfabeto.
Utilizamos String para poder desempaquetar las cadenas del procedimiento RSA.
-}
numeroALetraRSA :: Alfabeto -> String -> Char
numeroALetraRSA _ "99" = ' '
numeroALetraRSA sigma numStr =
  let n = (read numStr :: Int) - 11
   in if n >= 0 && n < length sigma
        then sigma !! n
        else '?'

{-|
Calcula dinamicamente el tamano maximo de un bloque de cifrado RSA
para asegurar que el bloque en texto plano siempre sea menor que N.
Ej: Si N=3233 (4 digitos), los bloques deben ser de 2 digitos.
-}
tamanoBloque :: Integer -> Int
tamanoBloque n =
  let k = length (show n)
      l = k - 1
   in if even l then l else l - 1

{-|
Parte una cadena en pedazos de tamano k.
-}
partirEnBloques :: Int -> String -> [String]
partirEnBloques _ [] = []
partirEnBloques k xs = take k xs : partirEnBloques k (drop k xs)

{-|
Genera la clave privada (n, d) a partir de los numeros primos p y q, y el exponente publico e.
Utiliza el inverso modular para encontrar d tal que e * d = 1 (mod phi(n)).
-}

calcularClavePrivada :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
calcularClavePrivada p q e =
  let n = p * q
      phi = (p - 1) * (q - 1)
   in case inversoModular e phi of
        Just d -> Just (n, d)
        Nothing -> Nothing

{-|
Convierte un numero entero a una cadena de letras de tamano fijo K usando la codificacion en Base-L.
(Donde L es la longitud del alfabeto). Esto transforma la basura numerica de RSA a texto legible.
-}
enteroABaseL :: Alfabeto -> Int -> Integer -> String
enteroABaseL sigma k num =
  let l = fromIntegral (length sigma)
      digitos = reverse $ take k $ map (\x -> fromInteger (x `mod` l)) (iterate (`div` l) num)
   in map (sigma !!) digitos

{-|
Convierte una cadena de letras en Base-L matematicamente de regreso a un numero entero gigante.
-}
baseLAEntero :: Alfabeto -> String -> Integer
baseLAEntero sigma cadena =
  let l = fromIntegral (length sigma)
      obtenerIndice c = case indiceLetra sigma c of
        Just i -> fromIntegral i
        Nothing -> 0
      indices = map obtenerIndice cadena
   in foldl (\acc d -> acc * l + d) 0 indices

{-|
Calcula dinamicamente el tamano maximo de letras que ocupamos en Base-L
para poder representar un numero menor a N sin que se desborde.
-}
longitudBaseL :: Alfabeto -> Integer -> Int
longitudBaseL sigma n =
  let l = fromIntegral (length sigma)
   in length (takeWhile (<= n) (iterate (* l) 1))

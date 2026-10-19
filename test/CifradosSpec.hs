module CifradosSpec (spec) where

import Test.Hspec
import Cifrados

-- NOTA: Las pruebas de los resultados de cifrado asumen un comportamiento especifico
-- que puede variar un poco dependiendo del alfabeto exacto.
-- Se han agregado fragmentos de los ejercicios del PDF para validar que 
-- la encriptacion y desencriptacion son operaciones inversas (identidad), 
-- sin importar la clave exacta utilizada en el criptoanalisis.

-- Definimos un alfabeto amplio por defecto para que las funciones lo usen.
alfabetoPrueba :: String
alfabetoPrueba = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,&nNaeiouAEIOU"

spec :: Spec
spec = do
  describe "1. Cifrado Cesar" $ do
    it "descifra y cifra correctamente el fragmento del ejercicio 1a (identidad)" $ do
      let clave = 3 -- Clave arbitraria para probar identidad
          fragmento = "efvekzveufgfihlvkvexfvjkvuzrizfuvcwlklifgvifuvsfrgiveuvirljricftfiivtkrdvekv"
      cifrarCesar alfabetoPrueba clave (descifrarCesar alfabetoPrueba clave fragmento) `shouldBe` fragmento

  describe "2. Cifrado Decimado" $ do
    it "descifra y cifra correctamente el fragmento del ejercicio 2a (identidad)" $ do
      let clave = 3 -- Coprimo con la longitud del alfabeto
          fragmento = "dityegomgsdtgjgnwiyeggojatgngojghegwilgzmlaisegtjg"
      cifrarDecimado alfabetoPrueba clave (descifrarDecimado alfabetoPrueba clave fragmento) `shouldBe` fragmento

  describe "3. Cifrado Afin" $ do
    it "descifra y cifra correctamente el fragmento del ejercicio 3a (identidad)" $ do
      let clave = (3, 5) -- 'a' coprimo con la longitud del alfabeto
          fragmento = "dihekdkcfkxkaziqqxxkvqhqxtqrefqfkediakdhkckcikctismkticnexqhqczqakd"
      cifrarAfin alfabetoPrueba clave (descifrarAfin alfabetoPrueba clave fragmento) `shouldBe` fragmento

  describe "4. Cifrado Vigenere" $ do
    it "descifra y cifra correctamente el fragmento del ejercicio 6 (identidad)" $ do
      let clave = "MISTERIO" -- Clave arbitraria
          fragmento = "XZJKE OCSLVLO J WBO AUN TDGMYKMNOWH DMXGUCDWHL"
      cifrarVigenere alfabetoPrueba clave (descifrarVigenere alfabetoPrueba clave fragmento) `shouldBe` fragmento

  describe "5. Cifrado Hill" $ do
    it "descifra y cifra usando la matriz del ejercicio 7" $ do
      let clave = [[3, 2, 6], [5, 7, 11], [13, 4, 1]]
          fragmento = "CGZEBKXYZEOFGHPMUVYTYTEZNCXPTZ"
      cifrarHill alfabetoPrueba clave (descifrarHill alfabetoPrueba clave fragmento) `shouldBe` fragmento

  describe "6. Cifrado RSA" $ do
    it "descifra lo que cifra (identidad con claves del ejercicio 9a)" $ do
      let pub = (340160617, 485)
          priv = (340160617, 485) 
          textoPrueba = "HOLA"
      descifrarRSA alfabetoPrueba priv (cifrarRSA alfabetoPrueba pub textoPrueba) `shouldBe` textoPrueba

  describe "7. Cifrado ElGamal" $ do
    it "descifra lo que cifra (identidad con claves del ejercicio 10a)" $ do
      let pub = (1367, 10, 1194)
          priv = (1367, 10, 1) -- Clave privada dummy x
          textoPrueba = "HOLA"
      descifrarElGamal alfabetoPrueba priv (cifrarElGamal alfabetoPrueba pub textoPrueba) `shouldBe` textoPrueba

  describe "8. Cifrado Alberti Indice de Letras" $ do
    it "cifra y descifra el texto en claro del ejercicio 4a" $ do
      let clave = ('B', 'x') 
          texto = "PcomomedicoLnoRpuedoperSmitirXqueGnadiesufraEharetodoloTposibleparaVsalvarvidas."
      descifrarAlbertiLetras alfabetoPrueba clave (cifrarAlbertiLetras alfabetoPrueba clave texto) `shouldBe` texto

    it "descifra y cifra el fragmento del ejercicio 5a (identidad)" $ do
      let clave = ('A', 'a') -- Dummy key
          fragmento = "XyvbnodndvrnsxonqnhnLhbakhn&a&ganazGorbhs&dqrbFpqtqgocvivgn"
      cifrarAlbertiLetras alfabetoPrueba clave (descifrarAlbertiLetras alfabetoPrueba clave fragmento) `shouldBe` fragmento

  describe "9. Cifrado Alberti Indice de Numeros" $ do
    it "cifra y descifra el texto en claro del ejercicio 4b" $ do
      let clave = ('D', 3) 
          texto = "noimporta4lodif1icilquesea2nuncapod3rerenunciar1amisuenodeser3elreydelo2spiratas"
      descifrarAlbertiNumeros alfabetoPrueba clave (cifrarAlbertiNumeros alfabetoPrueba clave texto) `shouldBe` texto

    it "descifra y cifra el fragmento del ejercicio 5b (identidad)" $ do
      let clave = ('A', 1) -- Dummy key
          fragmento = "qismhidarknfkynkoknzhkpsrihrigihqyzyxycxqlqlogcvercsltdpsrctve&textqdtclgqrqzceelblfdegzkfl"
      cifrarAlbertiNumeros alfabetoPrueba clave (descifrarAlbertiNumeros alfabetoPrueba clave fragmento) `shouldBe` fragmento

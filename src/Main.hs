{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cifrados
import Cifrados.Utilidades (calcularClavePrivada)
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Info (os)
import System.Process (callCommand)
import Data.Char (toLower)
import Text.Read (readMaybe)
import Control.Exception (try, SomeException, evaluate)
import Data.List (foldl')
main :: IO ()
main = do
  _ <- forkIO abrirNavegador
  startGUI defaultConfig {jsAddr = Just "0.0.0.0"} configurar

abrirNavegador :: IO ()
abrirNavegador = do
  threadDelay 1000000
  let url = "http://localhost:8023"
      comando = case os of
        "mingw32" -> "start " ++ url
        "darwin" -> "open " ++ url
        _ -> "xdg-open " ++ url
  callCommand comando

configurar :: Window -> UI ()
configurar ventana = do
  return ventana # set title "Herramienta de Cifrado Teoria de numeros 2"


  let css =
        "body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color: #1a202c; padding: 20px; color: #e2e8f0; } "
          ++ ".contenedor { max-width: 800px; margin: 0 auto; background: #2d3748; padding: 30px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3); } "
          ++ "h1 { color: #f7fafc; text-align: center; border-bottom: 2px solid #4a5568; padding-bottom: 15px; margin-bottom: 20px; } "
          ++ ".grupo { margin-bottom: 20px; } "
          ++ "label { display: block; font-weight: 600; margin-bottom: 8px; color: #cbd5e0; } "
          ++ "input:not([type='checkbox']), select { width: 100%; padding: 10px; background-color: #4a5568; color: #f7fafc; border: 1px solid #718096; border-radius: 6px; box-sizing: border-box; font-size: 16px; } "
          ++ "input:not([type='checkbox']):focus, select:focus { outline: none; border-color: #63b3ed; box-shadow: 0 0 0 3px rgba(99, 179, 237, 0.3); } "
          ++ ".botones { display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 10px; margin-top: 15px; } "
          ++ "button { background-color: #1c39bb; color: white; border: none; padding: 12px; border-radius: 6px; font-weight: bold; cursor: pointer; transition: background 0.2s; box-shadow: 0 2px 4px rgba(0,0,0,0.2); } "
          ++ "button:hover { background-color: #0f1f7a; transform: translateY(-1px); } "
          ++ "button:active { transform: translateY(1px); } "
          ++ ".resultado { margin-top: 30px; padding: 20px; background-color: #1a202c; border-left: 5px solid #63b3ed; border-radius: 4px; font-family: monospace; font-size: 16px; min-height: 80px; word-break: break-all; color: #63b3ed; font-weight: bold; } "
          ++ ".switch-container { display: flex; align-items: center; margin-bottom: 20px; background: #334155; padding: 15px; border-radius: 8px; } "
          ++ ".switch-label { font-weight: 600; color: #e2e8f0; margin-right: auto; } "
          ++ ".switch { position: relative; display: inline-block; width: 60px; height: 34px; margin-bottom: 0; } "
          ++ ".switch input { opacity: 0; width: 0; height: 0; } "
          ++ ".slider { position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0; background-color: #4a5568; transition: .4s; border-radius: 34px; } "
          ++ ".slider:before { position: absolute; content: ''; height: 26px; width: 26px; left: 4px; bottom: 4px; background-color: white; transition: .4s; border-radius: 50%; } "
          ++ "input:checked + .slider { background-color: #1c39bb; } "
          ++ "input:checked + .slider:before { transform: translateX(26px); }"

  styleTag <- UI.mkElement "style" # set text css
  getHead ventana #+ [element styleTag]

  encabezado <- UI.h1 # set text "Herramienta de Cifrado/Descifrado"

  -- Datos de la materia
  datosMateria <- UI.div # set style [("background-color", "#334155"), ("padding", "15px"), ("border-radius", "8px"), ("margin-bottom", "20px"), ("color", "#e2e8f0")] #+
      [ UI.p # set html "<b>Materia:</b> Teoria de Numeros II<br><b>Profesores:</b><br>&nbsp;&nbsp;Gabriela Cervantes Piza<br>&nbsp;&nbsp;Sergei Giovanni Quintero Villeda<br><b>Alumno:</b> Ian Alejandro Arroyo Ramírez" ]

  -- Ejemplos de uso
  ejemplosUso <- UI.div # set style [("background-color", "#334155"), ("padding", "15px"), ("border-radius", "8px"), ("margin-bottom", "20px"), ("color", "#e2e8f0"), ("font-size", "14px"), ("line-height", "1.5")] #+
      [ UI.h3 # set text "Guia Rapida de Claves:" # set style [("margin-top", "0"), ("border-bottom", "1px solid #4a5568"), ("padding-bottom", "5px")]
      , UI.p # set html "<b>Cesar:</b> <code>3</code><br>\
                        \<b>Decimado:</b> <code>5</code><br>\
                        \<b>Afin:</b> <code>(3, 5)</code><br>\
                        \<b>Vigenere:</b> <code>SECRETO</code><br>\
                        \<b>Hill:</b> <code>[[1,2],[3,4]]</code><br>\
                        \<b>RSA Cifrar:</b> <code>(899, 17)</code> o <code>(29, 31, 17)</code><br>\
                        \<b>RSA Descifrar:</b> <code>(899, 611)</code><br>\
                        \<b>ElGamal Cifrar:</b> <code>(101, 2, 5, 3)</code> o <code>(101, 2, 5)</code><br>\
                        \<b>ElGamal Descifrar:</b> <code>(101, 24)</code><br>\
                        \<b>Alberti (Letras):</b> <code>(A, s)</code><br>\
                        \<b>Alberti (Numeros):</b> <code>(D, h)</code>"
      ]

  etiquetaAlfabeto <- UI.label # set text "Alfabeto de cifrado:"
  entradaAlfabeto <- UI.input # set UI.value "abcdefghijklmnñopqrstuvwxyz"

  etiquetaEntrada <- UI.label # set text "Texto a cifrar / descifrar:"
  entradaTexto <- UI.input

  etiquetaClave <- UI.label # set text "Clave:"
  entradaClave <- UI.input # set (UI.attr "placeholder") "..."

  etiquetaOperacion <- UI.label # set text "Operacion:"
  selectOperacion <- UI.select #+ 
      [ UI.option # set UI.value "cifrar" # set text "Cifrar"
      , UI.option # set UI.value "descifrar" # set text "Descifrar"
      ]

  etiquetaIgnorarMayus <- UI.span # set UI.class_ "switch-label" # set text "Normalizar texto (ignorar mayusculas/minusculas):"
  checkIgnorarMayus <- UI.input # set (UI.attr "type") "checkbox" # set UI.checked True
  sliderSpan <- UI.span # set UI.class_ "slider"
  switchLabel <- UI.label # set UI.class_ "switch" #+ [element checkIgnorarMayus, element sliderSpan]


  botonCesar <- UI.button # set text "Cesar"
  botonDecimado <- UI.button # set text "Decimado"
  botonAfin <- UI.button # set text "Afin"
  botonVigenere <- UI.button # set text "Vigenere"
  botonHill <- UI.button # set text "Hill"
  botonRSA <- UI.button # set text "RSA"
  botonElGamal <- UI.button # set text "ElGamal"
  botonAlbertiL <- UI.button # set text "Alberti (Letras)"
  botonAlbertiN <- UI.button # set text "Alberti (Numeros)"

  areaResultado <- UI.div # set UI.class_ "resultado" # set text "El resultado de la operacion aparecera aqui..."


  let botones = [botonCesar, botonDecimado, botonAfin, botonVigenere, botonHill, botonRSA, botonElGamal, botonAlbertiL, botonAlbertiN]

  let contenedorBotones = UI.div # set UI.class_ "botones" #+ map element botones

      grupoAlfabeto = UI.div # set UI.class_ "grupo" #+ [element etiquetaAlfabeto, element entradaAlfabeto]
      grupoTexto = UI.div # set UI.class_ "grupo" #+ [element etiquetaEntrada, element entradaTexto]
      grupoClave = UI.div # set UI.class_ "grupo" #+ [element etiquetaClave, element entradaClave]
      grupoOperacion = UI.div # set UI.class_ "grupo" #+ [element etiquetaOperacion, element selectOperacion]
      grupoOpciones = UI.div # set UI.class_ "switch-container" #+ [element etiquetaIgnorarMayus, element switchLabel]

      tituloBotones = UI.h3 # set text "Selecciona el metodo de cifrado:" # set style [("color", "#e2e8f0"), ("margin-top", "30px"), ("font-size", "18px")]

      contenedorPrincipal =
        UI.div
          # set UI.class_ "contenedor"
          #+ [ element encabezado,
               element datosMateria,
               element ejemplosUso,
               grupoAlfabeto,
               grupoTexto,
               grupoClave,
               grupoOperacion,
               grupoOpciones,
               tituloBotones,
               contenedorBotones,
               element areaResultado
             ]

  getBody ventana #+ [contenedorPrincipal]


  

  UI.on UI.click botonCesar $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      ignorar <- UI.get UI.checked checkIgnorarMayus
      
      let normalizar = if ignorar then map toLower else id
          alf = normalizar alfRaw
          texto = normalizar textoRaw
          claveStr = normalizar claveStrRaw
          
      let claveM = readMaybe claveStr :: Maybe Int
      let resultado = case claveM of
                        Just clave -> if operacion == "cifrar"
                                      then cifrarCesar alf clave texto
                                      else descifrarCesar alf clave texto
                        Nothing -> "Error: La clave debe ser un numero entero valido."
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonDecimado $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      ignorar <- UI.get UI.checked checkIgnorarMayus
      
      let normalizar = if ignorar then map toLower else id
          alf = normalizar alfRaw
          texto = normalizar textoRaw
          claveStr = normalizar claveStrRaw
      
      let claveM = readMaybe claveStr :: Maybe Int
      let resultado = case claveM of
                        Just clave -> if operacion == "cifrar"
                                      then cifrarDecimado alf clave texto
                                      else descifrarDecimado alf clave texto
                        Nothing -> "Error: La clave debe ser un numero entero valido."
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonAfin $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      ignorar <- UI.get UI.checked checkIgnorarMayus
      
      let normalizar = if ignorar then map toLower else id
          alf = normalizar alfRaw
          texto = normalizar textoRaw
          claveStr = normalizar claveStrRaw
      

      let claveM = readMaybe claveStr :: Maybe (Int, Int)
      let resultado = case claveM of
                        Just clave -> if operacion == "cifrar"
                                      then cifrarAfin alf clave texto
                                      else descifrarAfin alf clave texto
                        Nothing -> "Error: La clave debe ser una tupla valida de dos numeros enteros, ej. (3, 5)."
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonVigenere $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      ignorar <- UI.get UI.checked checkIgnorarMayus
      
      let normalizar = if ignorar then map toLower else id
          alf = normalizar alfRaw
          texto = normalizar textoRaw
          claveStr = normalizar claveStrRaw
      
      let resultado = if operacion == "cifrar"
                      then cifrarVigenere alf claveStr texto
                      else descifrarVigenere alf claveStr texto
                      
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonHill $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      ignorar <- UI.get UI.checked checkIgnorarMayus
      
      let normalizar = if ignorar then map toLower else id
          alf = normalizar alfRaw
          texto = normalizar textoRaw
          claveStr = normalizar claveStrRaw
      

      let claveM = readMaybe claveStr :: Maybe [[Int]]
      let resultado = case claveM of
                        Just clave -> if operacion == "cifrar"
                                      then cifrarHill alf clave texto
                                      else descifrarHill alf clave texto
                        Nothing -> "Error: La clave debe ser una matriz valida, ej. [[1,2],[3,4]]."
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonRSA $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      ignorar <- UI.get UI.checked checkIgnorarMayus
      
      let normalizar = if ignorar then map toLower else id
          alf = normalizar alfRaw


          texto = normalizar textoRaw
          claveStr = normalizar claveStrRaw
      

      let clave3 = readMaybe claveStr :: Maybe (Integer, Integer, Integer)
          clave2 = readMaybe claveStr :: Maybe (Integer, Integer)
      
      let resultado = case (clave3, clave2) of
            (Just (p, q, e), _) -> 
                let n = p * q
                in if operacion == "cifrar"
                   then cifrarRSA alf (n, e) texto
                   else case calcularClavePrivada p q e of
                          Just clavePriv -> descifrarRSA alf clavePriv texto
                          Nothing -> "Error: El exponente 'e' no tiene inverso modular (no son coprimos)."
            
            (_, Just (n, e_o_d)) ->
                if operacion == "cifrar"
                then cifrarRSA alf (n, e_o_d) texto
                else descifrarRSA alf (n, e_o_d) texto
                
            (Nothing, Nothing) -> "Error: Formato de clave invalido. Ingresa (n, e) o (p, q, e)."
                      
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonElGamal $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      ignorar <- UI.get UI.checked checkIgnorarMayus
      
      let normalizar = if ignorar then map toLower else id
          alf = normalizar alfRaw
          texto = normalizar textoRaw
          claveStr = normalizar claveStrRaw
          
      let clave4 = readMaybe claveStr :: Maybe (Integer, Integer, Integer, Integer)
          clave3 = readMaybe claveStr :: Maybe (Integer, Integer, Integer)
          clave2 = readMaybe claveStr :: Maybe (Integer, Integer)
          
      let resultado = case operacion of
            "cifrar" -> case clave4 of
                Just (p, g, a, k) -> cifrarElGamal alf (p, g, a) k texto
                Nothing -> case clave3 of
                    Just (p, g, a) -> cifrarElGamal alf (p, g, a) 3 texto
                    Nothing -> "Error: Para cifrar ElGamal usa (p, g, a, k) o (p, g, a)."
            "descifrar" -> case clave2 of
                Just (p, e) -> descifrarElGamal alf (p, e) texto
                Nothing -> "Error: Para descifrar ElGamal usa (p, e)."
                
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonAlbertiL $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      
      let claveM = parseAlberti claveStrRaw
          
      let resultado = case claveM of
            Just clave -> if operacion == "cifrar"
                          then cifrarAlbertiLetras alfRaw clave textoRaw
                          else descifrarAlbertiLetras alfRaw clave textoRaw
            Nothing -> "Error: La clave debe ser de la forma (A, s)"
                
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro


  UI.on UI.click botonAlbertiN $ \_ -> do
      alfRaw <- UI.get UI.value entradaAlfabeto
      textoRaw <- UI.get UI.value entradaTexto
      claveStrRaw <- UI.get UI.value entradaClave
      operacion <- UI.get UI.value selectOperacion
      
      let claveM = parseAlberti claveStrRaw
          
      let resultado = case claveM of
            Just clave -> if operacion == "cifrar"
                          then cifrarAlbertiNumeros alfRaw clave textoRaw
                          else descifrarAlbertiNumeros alfRaw clave textoRaw
            Nothing -> "Error: La clave debe ser de la forma (D, h)"
                
      resultadoSeguro <- liftIO (seguroEval resultado)
      element areaResultado # set text resultadoSeguro

  return ()

seguroEval :: String -> IO String
seguroEval s = do
  res <- try (evaluate (foldl' (\acc c -> c `seq` acc) () s `seq` s)) :: IO (Either SomeException String)
  case res of
    Left _ -> return "❌ Error: Operación inválida. Revisa que todos los caracteres de tu texto existan en el alfabeto o que la clave sea correcta."
    Right val -> return val

parseAlberti :: String -> Maybe (Char, Char)
parseAlberti str = 
  let chars = filter (\c -> c /= '(' && c /= ')' && c /= ' ' && c /= ',' && c /= '\'') str
  in if length chars >= 2 
     then Just (chars !! 0, chars !! 1) 
     else Nothing

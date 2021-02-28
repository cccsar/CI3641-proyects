module Main (main) where

-- Imports locales 
import Oraculo

-- Imports de haskell
import qualified Text.Read  as R (readMaybe)
import qualified Data.Map   as M (toList, keys, fromList, lookup, Map, insert)
import qualified Data.Maybe as MB(fromJust)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout, stderr, hPutStrLn )
import Control.Monad(unless)


-- inicializa un oraculo arbitrario e inicia el cliente con él
main = initHsk >>= client


{- funciones auxiliares -}

-- Parsea el input de usuario constantemente y ejecuta 
-- las acciones que se le soliciten
client :: Oraculo -> IO ()
client or = do
  putStrLn introduction
  opt <- askOptions (names ++ [exit])

  let f = MB.fromJust . M.lookup opt $ nameToFunc

  unless (opt==exit) $ f or >>= client

  where
    functions :: M.Map String (Oraculo -> IO  Oraculo)
    functions = M.fromList [("predecir", predict)]

initHsk :: IO Oraculo
initHsk = do
  oraculo <- loadFromFile firstOraculo

  case oraculo of
    Nothing -> return (Prediccion "42") 
    Just o  -> return o

{----------- Funciones principales -----------}

-- Función de predicción como fue descrita en el pdf 
predict :: Oraculo -> IO Oraculo
predict q@Pregunta{pregunta = p, opciones = opts} = do
  let oraculoOpts = M.keys opts             -- opciones del oraculo
  let userOpts = oraculoOpts ++ [noOption]  -- Todas las opciones, incluyendo "ninguna"

  -- prompt for an answer
  putStrLn $ '¿':p ++ "?"
  opt <- askOptions userOpts

  if opt == noOption
    then askValidOption q
    else do
      newOraculo <- predict $ respuesta q opt
      return q{opciones = M.insert opt newOraculo opts}

  where
    askValidOption :: Oraculo -> IO Oraculo -- (pregunta, respuesta)
    askValidOption q@Pregunta{opciones=opts}= do
      -- imprimir que haskinator perdió
      putStrLn failMessage

      -- Pedir la opcion que esperaba
      option <- getInLine askForExpectedOption

      -- Pedir la respuesta que esperaba
      answer <- getInLine askForExpectedAnswer

      -- Actualizar este oraculo con una nueva opcion respuesta
      let newOpts = M.insert option (Prediccion answer) opts

      return q{opciones=newOpts}


predict prd@(Prediccion p) = do
  -- Imprimir predicción
  putStrLn $ predictionStr p
  let opts = [yes,no]
  selection <- askOptions opts

  if selection == yes
    then return prd -- Si ganamos, el oraculo se mantiene
    else putStrLn failMessage >> askNewQuestion
  where
    askNewQuestion :: IO Oraculo
    askNewQuestion = do
      -- solicita la respuesta correcta
      actualAns <- getInLine askForExpectedAnswer

      -- Solicita una pregunta suya respuesta nos llevaría a la verdadera respuesta
      importantQuestion <- getInLine askForImportatQuestion

      -- Solicita una opción que debería llevarnos a la respuesta correcta
      newOption <- getInLine askOption

      -- Solicita una opción que debería llevarnos a nuestra respuesta actual
      newOptionForOldAns <- getInLine . askOptionForInvalidAns $ p

      return $ ramificar [newOptionForOldAns, newOption] [prd, Prediccion actualAns] importantQuestion


-- Crea un oraculo nuevo desde una prediccion dada.
create :: Oraculo -> IO Oraculo
create _ = do
  -- get a new prediction
  prd <- getInLine askForNewPrediction

  return $ Prediccion prd


-- Solicita dos strings correspondientes a predicciones en el oraculo y dice que pregunta llevaria a 
-- ambas predicciones.
importantQuestion :: Oraculo -> IO Oraculo
importantQuestion sybil = do
  putLine "Dame dos strings que representen predicciones!. Buscare la pregunta que tienen en comun."

  first  <- getInLine' "Primer String: "
  second <- getInLine' "Segundo String: "

  let out = lcaOraculo first second sybil

  case out of
    Nothing -> do
     putLine "Consulta invalida. Debe ingresar dos strings que representen predicciones en el oraculo"
     return sybil
    Just x  -> do
     putLine $ "La pregunta que lleva a las predicciones: "++"\""++first++"\" y \""++second++"\" es:"
     putStrLn ("\t\t"++x)
     return sybil


--Guarda una representacion textual del oraculo a un archivo.
persist :: Oraculo -> IO Oraculo
persist sybil = do
  name <- getInLine "Dame un nombre de archivo para guardar al oraculo: "

  writeFile name (show sybil++"\n")

  return sybil


--Carga de un archivo una representacion textual de un oraculo.
load :: Oraculo -> IO Oraculo
load sybil = do
  name <- getInLine "Dame un nombre de archivo de donde cargar el oraculo"

  content <- loadFromFile name
  case content of
    Nothing -> do
      putLine "No pude leer un oraculo de ese archivo" >> return sybil
    Just f -> return f

  --if cond then do
  --  content <- readFile name
  --  return (read content)
  --else do
  --  putLine "El nombre de archivo proporcionado no corresponde a un archivo existente."
  --  return sybil


{----------- Utilidades -----------}

-- Permite mostrar un string y solicitar input en la misma linea
getInLine :: String -> IO String
getInLine ss = do
  putStrLn ss
  putStr $ prompt ++ " "
  hFlush stdout
  inp <- getLine
  hFlush stdout
  return inp

-- Similar a la funcion anterior pero sin el prompt de haskinator
getInLine' :: String -> IO String
getInLine' ss = do
  putStr ss
  hFlush stdout
  getLine


-- Imprime un mensaje de haskinator
putLine :: String -> IO ()
putLine ss =  putStrLn (prompt ++ (' ':ss))


-- Muestra una lista de opciones, esperando recibir un número correspondiente
-- a la opción
askOptions :: [String] -> IO String
askOptions opts =
  do
    let
      getAns :: IO String
      getAns = do
        ans <- getInLine answers
        case R.readMaybe ans of
          Nothing -> putStrLn invalidInput >> getAns
          Just i  -> if i > length opts then putStrLn optionOutOfRange >> getAns
                                        else return $ opts!!(i-1)
    getAns
  where
    answers =
      unlines $
        zipWith
          (\i opt -> "  " ++ show i ++ ") " ++ opt)
          [1..]
          opts


-- Funcion para imprimir una representacion decente de las opciones
-- y pedir una respuesta
prettyOptions :: Oraculo -> IO String
prettyOptions (Pregunta s opts) = do
  putStrLn s
  getInLine answers
 where
  answers =
    unlines $
      zipWith
        (\i opt -> "  " ++ show i ++ ") " ++ opt)
        [1..]
        (M.keys opts)


-- Calcula el lca para dos strings correspondientes a dos predicciones
-- en un oraculo.
lcaOraculo :: String -> String -> Oraculo -> Maybe String
lcaOraculo firstS secondS o =
   case result of
     [a,b] -> Just (fst . last . takeWhile (uncurry (==)) . zip (reverse a) $ reverse b)
     _     -> Nothing
 where
  result = lookupT [] firstS secondS o

  -- lookupT :: [String] -> String -> String -> Oraculo -> [[String]]
  lookupT path a b (Prediccion s)
    | a == s || b == s = [ s:path ]
    | otherwise        = []
  lookupT path a b (Pregunta s mp)
    | a == s || b == s = [ s:path ]
    | otherwise        = concatMap (lookupT (s : path) a b . snd) (M.toList mp)


loadFromFile :: String -> IO (Maybe Oraculo)
loadFromFile f = do
  cond <- doesFileExist f

  if cond then do
    content <- readFile f
    return (R.readMaybe content)
  else return Nothing 

{----------- Constantes y strings -----------}

nameToFunc :: M.Map String (Oraculo -> IO Oraculo)
nameToFunc = M.fromList [
    ("Predecir", predict),
    ("Crear", create),
    ("Cargar", load),
    ("Persistir", persist),
    ("Pregunta crucial", importantQuestion)
  ]

names = M.keys nameToFunc

exit = "salir"

-- Oraculo inicial
firstOraculo :: String 
firstOraculo = "p1.txt"

-- String de presentacion
introduction :: String
introduction = " Bienvenido a mi humilde morada viajero!\n ¿Estas listo para presenciar las increibles"++
               " pero efectivas habilidades de este viejo oráculo?"

-- Nombres de opciones
nombres :: [String]
nombres = ["crear","predecir","persistir","cargar","consultar"]

-- Prompt de input invalido
invalidInput :: String
invalidInput = "Lo siento, no pude entenderte. Intenta de nuevo"

-- Input number out of range
optionOutOfRange :: String
optionOutOfRange = "Esa no es una opción válida. Intenta de nuevo"

-- Prompt a usar 
prompt :: String
prompt = "Haskinator>"

-- Literal "ninguna opcion"
noOption :: String
noOption = "Ninguna"

-- Imprimir prediccion
predictionStr :: String -> String
predictionStr s = "Estabas pensando en: " ++ s ++ ", ¿no es así?"

-- si y no
yes :: String
yes = "Sí"
no  = "No"

-- String para solicitar la opcion y respuesta esperada
askForExpectedOption :: String
askForExpectedOption = "¿Qué opción esperabas?"

askForExpectedAnswer :: String
askForExpectedAnswer = "¿En qué estabas pensando?"

-- Mensajes de ganar y perder
winMessage :: String
winMessage = "Lo sabia, no eres rival para mis poderes"

failMessage :: String
failMessage = "Parece que has logrado superar mis poderes."

-- Solicita informacion cuando se falla una predicción
askForImportatQuestion :: String
askForImportatQuestion = "¿Qué pregunta me habría llevado a esa respuesta?"

askOption :: String
askOption = "¿Qué opción me llevaría a esa respuesta?"

askOptionForInvalidAns :: String -> String
askOptionForInvalidAns s = "¿Qué opción me debería llevar a '" ++ s ++ "'?"

-- Solicitar una prediccion para crear oraculo nuevo
askForNewPrediction :: String
askForNewPrediction = "Dame una predicción, lo primero que se te ocurra"

-- Pregunta de rutina en dialogo repetitivo
preface :: String
preface = " Dime, ¿qué deseas hacer?"

module Main (main) where

-- Local imports 
import Oraculo

-- Haskell imports
import Data.Char (toLower)
import qualified Text.Read  as R (readMaybe)
import qualified Data.Map   as M (toList, keys, fromList, lookup, Map, insert)
import qualified Data.Maybe as MB(fromJust)
import System.IO (hFlush, stdout, stderr, hPutStrLn )
import System.Exit
import Control.Monad(unless)


main = client (Prediccion "42")

{- Helper functions -}
client :: Oraculo -> IO ()
client or = do
  opt <- askOptions names

  let f = MB.fromJust . M.lookup opt $ nameToFunc

  unless (opt=="salir") $ f or >>= client
  
  where
    functions :: M.Map String (Oraculo -> IO  Oraculo)
    functions = M.fromList [("predecir", predict)]

{----------- Main Functions -----------}

-- Prediction function as described in the pdf 
predict :: Oraculo -> IO Oraculo
predict q@Pregunta{pregunta = p, opciones = opts} = do
  let oraculoOpts = M.keys opts             -- oraculo options
  let userOpts = oraculoOpts ++ [noOption]  -- all options (including no option at all)

  -- prompt for an answer
  putStrLn p
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
  -- print prediction
  putStrLn $ predictionStr p
  let opts = [yes,no]
  selection <- askOptions opts

  if selection == yes
    then return prd -- If we win, keep the same oraculo 
    else putStrLn failMessage >> askNewQuestion
  where
    askNewQuestion :: IO Oraculo
    askNewQuestion = do
      -- Ask for the correct answer
      actualAns <- getInLine askForExpectedAnswer

      -- ask a question such that its answer would lead to the actual answer
      importantQuestion <- getInLine askForImportatQuestion

      -- ask an option that should lead to the answer
      newOption <- getInLine askOption

      -- ask an option that should lead to our current answer
      newOptionForOldAns <- getInLine . askOptionForInvalidAns $ p

      return $ ramificar [newOptionForOldAns, newOption] [prd, Prediccion actualAns] importantQuestion

-- create a new oraculo from a given prediction.
create :: Oraculo -> IO Oraculo
create _ = do 
  -- get a new prediction
  prd <- getInLine askForNewPrediction
  
  return $ Prediccion prd

importantQuestion :: Oraculo -> IO Oraculo
importantQuestion sybil = do 
 putLine "Dame dos strings que representen predicciones!. Buscare la pregunta que tienen en comun."

 first  <- getInLine "Primer String: "
 second <- getInLine "Segundo String: "

 let out = lcaOraculo first second sybil 

 case out of 
  Nothing -> hPutStrLn stderr "Error: Consulta invalida. Por favor intente de nuevo" >> importantQuestion sybil
  Just x  -> do 
   putLine $ "La pregunta que lleva a las predicciones: "++"\""++first++"\" y \""++second++"\" es:"
   putStrLn ("\t\t"++x)
   return sybil

--Funciones relacionadas con manejo de archivos
persist' = undefined
load' = undefined
-- 

exit' _ = exitSuccess



{----------- Utils -----------}
-- Permite mostrar un string y solicitar input en la misma linea
getInLine :: String -> IO String
getInLine ss = do
  putStrLn ss
  putStr $ prompt ++ " "
  hFlush stdout
  inp <- getLine
  hFlush stdout
  return inp

-- Print a Haskinator message
putLine :: String -> IO () 
putLine ss =  putStrLn (prompt ++ (' ':ss)) 


-- show a list of options and return the selected option by asking for a
-- number 
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


-- Limpia la pantalla
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"


-- Se mueve a una posicion en la pantalla
goTo :: (Int,Int) -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


-- Limpia la pantalla y reajusta la posicion del prompt
begin :: IO ()
begin = clearScreen >> goTo (0,0) 


-- Calcula el lca para dos strings correspondientes a dos predicciones
-- en un oraculo.
lcaOraculo :: String -> String -> Oraculo -> Maybe String
lcaOraculo firstS secondS o = 
   case result of 
     [a,b] -> Just (fst . last . takeWhile (\(a,b) -> a == b) . zip (reverse a) $ (reverse b))
     _     -> Nothing
 where
  result = lookupT [] firstS secondS o 

  -- lookupT :: [String] -> String -> String -> Oraculo -> [[String]]
  lookupT path a b (Prediccion s)
    | a == s || b == s = [ s:path ] 
    | otherwise        = [] 
  lookupT path a b (Pregunta s mp) 
    | a == s || b == s = [ s:path ] 
    | otherwise        = concatMap (lookupT (s:path) a b) (map snd . M.toList $ mp) 


{----------- Constants -----------}

nameToFunc :: M.Map String (Oraculo -> IO Oraculo)
nameToFunc = M.fromList [
    ("Predecir", predict),
    ("Crear", create)
  ]

names = M.keys nameToFunc


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

exit _ = exitSuccess

-- No option literal
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

-- Winning and losing prompts
winMessage :: String
winMessage = "Lo sabia, no eres rival para mis poderes"

failMessage :: String
failMessage = "Parece que has logrado superar mis poderes."

-- Asking for information when you fail a prediction
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

module Main (main) where

-- Local imports 
import Oraculo

-- Haskell imports
import Data.Char (toLower)
import qualified Data.Map as M (toList)
import System.IO (hFlush, stdout, stderr, hPutStrLn )
import System.Exit


main = putStrLn introduction >> parser sampleOraculo_1  -- por ahora


{- Helper functions -}

-- Controla el flujo inicial de ejecucion
parser :: Oraculo -> IO ()
parser xd = do
  putLine preface
  mapM_ (putStrLn . ("* " ++)) nombres

  opt <- getInLine "Escoga una opcion: "

  let choice = lookup (map toLower opt) dispatch -- busca opcion en lista de asociacioens

  case choice of
    Nothing -> hPutStrLn stderr "Haskinator> Seleccione una opcion valida!" >> parser xd
    Just x  -> begin >> x xd


-- Permite mostrar un string y solicitar input en la misma linea
getInLine :: String -> IO String
getInLine ss = do
  putStr (prompt ++ (' ':ss) )
  inp <- getLine
  hFlush stdout
  return inp


-- Print a Haskinator message
putLine :: String -> IO () 
putLine ss =  putStrLn (prompt ++ (' ':ss)) 


-- Funcion para imprimir una representacion decente de las opcioens
prettyOptions :: Oraculo -> IO ()
prettyOptions (Pregunta s opts) = do
  getInLine s
  mapM_ putStrLn answers
 where
  answers = map fst . M.toList $ opts
  neat = map (++ "* ") answers


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


{- Necessary functions -}


create, predict, persist, load, importantQuestion, exit :: Oraculo -> IO () 


create = undefined

predict = undefined

persist = undefined

load = undefined

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
   parser sybil


exit _ = exitSuccess



{- Constants -}


-- Prompt a usar 
prompt :: String
prompt = "Haskinator>"


-- Lista de asociaciones para el parser: problema .. tipos distintos 
dispatch :: [ (String, Oraculo -> IO ()) ]
dispatch = zip nombres funciones


-- Nombres de las opciones del cliente (que se asocian a funciones)
nombres :: [String]
nombres = [ "crear", "predecir", "persistir", "cargar", "pregunta crucial", "salir" ]


-- Funciones que corresponden a las opciones del cliente
funciones :: [ Oraculo -> IO () ]
funciones = [ create, predict, persist, load, importantQuestion, exit]


-- Bienvenida al usuario
introduction = " Bienvenido a mi humilde morada viajero!\n ¿Estas listo para presenciar las increibles"++
               " pero efectivas habilidades de este viejo oráculo?"


-- Pregunta de rutina en dialogo repetitivo
preface :: String
preface = " Dime, ¿qué deseas hacer?" 

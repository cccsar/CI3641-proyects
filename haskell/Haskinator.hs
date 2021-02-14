module Main (main) where

-- Local imports 
import Oraculo

-- Haskell imports
import Data.Char (toLower)
import qualified Data.Map as M (toList)
import System.IO (hFlush, stdout, stderr, hPutStrLn )
import System.Exit


main = putStrLn introduction >> parser Nothing


{- Helper functions -}

-- Controla el flujo inicial de ejecucion
parser :: Maybe Oraculo -> IO ()
parser xd = do
  mapM_ (putStrLn . ("* " ++)) nombres
  opt <- getInLine "Escoga una opcion: "

  let choice = lookup (map toLower opt) dispatch -- busca opcion en lista de asociacioens

  case choice of
    Nothing -> hPutStrLn stderr "Haskinator> Seleccione una opcion valida!" >> parser xd
    Just x  -> undefined


-- Permite mostrar un string y solicitar input en la misma linea
getInLine :: String -> IO String
getInLine ss = do
  putStr (prompt ++ (' ':ss) )
  inp <- getLine
  hFlush stdout
  return inp


-- Funcion para imprimir una representacion decente de las opcioens
prettyOptions :: Oraculo -> IO ()
prettyOptions (Pregunta s opts) = do
  getInLine s
  mapM_ putStrLn answers
 where
  answers = map fst . M.toList $ opts
  neat = map (++ "* ") answers



{- Necessary functions -}

create, predict, persist, load, strangeQuery, exit :: Maybe Oraculo -> IO () 

create = undefined

predict = undefined

--Funciones relacionadas con manejo de archivos
persist = undefined

load = undefined
-- 

exit _ = exitSuccess

strangeQuery = undefined


{- Constants -}

-- Prompt a usar 
prompt :: String
prompt = "Haskinator>"

-- Lista de asociaciones para el parser: problema .. tipos distintos 
dispatch :: [ (String, Maybe Oraculo -> IO ()) ]
dispatch = zip nombres funciones

-- Nombres de las opciones del cliente (que se asocian a funciones)
nombres :: [String]
nombres = [ "crear", "predecir", "persistir", "cargar", "pregunta crucial", "salir" ]

-- Funciones que corresponden a las opciones del cliente
funciones :: [ Maybe Oraculo -> IO () ]
funciones = [ create, predict, persist, load, strangeQuery]

introduction = ""

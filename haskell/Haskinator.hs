module Haskinator (main) where

-- Por ahora dejare los imports implicitos para conocer el namespace

-- Local imports 
import Oraculo
import Constants as C

-- Haskell imports
import Data.Char (toLower)
import qualified Data.Map as M (toList)
import System.IO (hFlush, stdout, stderr, hPutStrLn )
import System.Exit

-- type State = (Oraculo, ... )  .. estado a definir que recibiran algunas funciones para trabajar con el oraculo 

main = putStrLn C.introduction >> parser Nothing

{- Helper functions -}

-- Controla el flujo inicial de ejecucion
parser :: Maybe Oraculo -> IO ()
parser xd = do
 mapM_ (putStrLn . (++ "* ")) nombres
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

create, predict, persist, load, strangeQuery, exit :: Maybe Oraculo -> IO () -- State -> IO () 

create = undefined

strangeQuery = undefined

predict = undefined

--Funciones relacionadas con manejo de archivos
persist = undefined
load = undefined
-- 

exit _ = exitSuccess




{- Constants -}

-- Prompt a usar 
prompt :: String
prompt = "Haskinator>"

-- Lista de asociaciones para el parser: problema .. tipos distintos 
dispatch :: [ (String, Maybe Oraculo -> IO ()) ]
dispatch = zip C.nombres funciones

funciones :: [ Maybe Oraculo -> IO () ]
funciones = [ create, predict, persist, load, strangeQuery]



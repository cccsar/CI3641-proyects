module Haskinator (main) where

-- Por ahora dejare los imports implicitos para conocer el namespace
import System.IO (hFlush, stdout, stderr, hPutStrLn, )
import Data.Char (toLower)
import Oraculo

-- type State = (Oraculo, ... )  .. estado a definir que recibiran algunas funciones para trabajar con el oraculo 

main = putStrLn introduction >> parser

{- Helper functions -} 

-- Controla el flujo inicial de ejecucion
parser :: IO () 
parser = do 
  mapM_ putStrLn (zipWith (++) (repeat "* ") nombres)
  opt <- safeReadInLine "Escoga una opcion: "

  let choice = lookup (map toLower opt) dispatch -- busca opcion en lista de asociacioens

  case choice of 
   Nothing -> hPutStrLn stderr "Haskinator> Seleccione una opcion valida!" >> parser
   Just x  -> undefined 


-- Muestra un prompt con el nombre de haskinator siempre que se solicite
-- input al usuario.

-- Permite mostrar un string y solicitar input en la misma linea
safeReadInLine :: String -> IO String
safeReadInLine ss = do 
  putStr (prompt ++ (' ':ss) )
  inp <- getLine
  hFlush stdout
  return inp 
  

{- Necessary functions -} 

create, predict, persist, load, strangeQuery, exit :: Oraculo -> IO () -- State -> IO () 

create = undefined

predict = undefined

persist = undefined

load = undefined

strangeQuery = undefined

exit = undefined


{- Constants -}

-- Prompt a usar 
prompt :: String
prompt = "Haskinator>"

-- Lista de asociaciones para el parser
dispatch :: [ (String,Oraculo -> IO ()) ]
dispatch = zip nombres funciones

nombres :: [String] 
nombres = ["crear","predecir","persistir","cargar","consultar"]

funciones :: [ Oraculo -> IO () ] 
funciones = [ create, predict, persist, load, strangeQuery, exit ] 

-- String de presentacion
introduction :: String
introduction = "This is Haskinator! the great oracle!"

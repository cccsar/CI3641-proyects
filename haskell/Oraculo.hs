module Oraculo where

import qualified Data.Map as M
 

data Oraculo = Prediccion {prediccion :: String} | Pregunta {pregunta :: String, opciones :: Opciones}

type Opciones = M.Map String Oraculo

-- Faltan las instancias

{- construccion -} 

-- Creates an oracle from a String prediction
crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

{- acceso -}
respuesta :: Oraculo -> String -> Oraculo
respuesta Pregunta{opciones=opc} s = 
    let fromJust Nothing   = error "Unexpected" 
        fromJust (Just xd) = xd
    in fromJust $ M.lookup s opc 

respuesta _                _ = error "respuesta: invalid value constructor."
-- Es seguro que el string que pasa esta entre las opciones?

{- modificacion -}

-- creates an answer 
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opc ors preg = Pregunta preg opcs
    where opcs = M.fromList $ zip opc ors

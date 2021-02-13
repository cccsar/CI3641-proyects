module Oraculo where

import qualified Data.Map as M


data Oraculo = Prediccion String | Pregunta String Opciones

type Opciones = M.Map String Oraculo

-- Faltan las instancias

{- construccion -} 

-- Creates an oracle from a String prediction
crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

{- acceso -}

-- returns a prediction in case the oracle is a prediction
prediccion :: Oraculo -> String
prediccion (Prediccion p) = p 
prediccion _              = error "prediccion: invalid value constructor."

-- returns an answer in case the oracle corresponds to an answer
pregunta :: Oraculo -> String
pregunta (Pregunta p _) = p
pregunta _              = error "pregunta: invalid value constructor."

-- returns optinos in case the oracle is an answer
opciones :: Oraculo -> Opciones
opciones (Pregunta _ opc) = opc -- map fst . M.toList $ opc
opciones _                = error "opciones: invalid value constructor."

respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta _ opc) s = let fromJust Nothing   = error "Unexpected" 
                                   fromJust (Just xd) = xd
                               in fromJust $ M.lookup s opc 
respuesta _                _ = error "respuesta: invalid value constructor."
-- Es seguro que el string que pasa esta entre las opciones?


{- modificacion -}

-- creates an answer 
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opc ors preg = Pregunta preg opcs
 where opcs = M.fromList $ zip opc ors

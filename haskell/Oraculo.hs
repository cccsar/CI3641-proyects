module Oraculo where

import qualified Data.Map as M


data Oraculo = Prediccion String | Pregunta String Opciones

type Opciones = M.Map String Oraculo

-- Faltan las instancias

-- acceso

crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

prediccion :: Oraculo -> String
prediccion (Prediccion p) = p 
prediccion _              = error "prediccion: invalid value constructor."

pregunta :: Oraculo -> String
pregunta (Pregunta p _) = p
pregunta _              = error "pregunta: invalid value constructor."

opciones :: Oraculo -> Opciones
opciones (Pregunta _ opc) = opc -- map fst . M.toList $ opc
opciones _                = error "opciones: invalid value constructor."

-- Es seguro que el string que pasa esta entre las opciones?
respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta _ opc) s = let fromJust Nothing   = error "Unexpected" 
                                   fromJust (Just xd) = xd
                               in fromJust $ M.lookup s opc 
respuesta _                _ = error "respuesta: invalid value constructor."

-- modif

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opc ors preg = Pregunta preg opcs
 where opcs = M.fromList $ zip opc ors

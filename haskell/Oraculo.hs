module Oraculo where

import qualified Data.Map as M
 
data Oraculo = Prediccion {prediccion :: String} 
             | Pregunta {pregunta :: String, opciones :: Opciones}

type Opciones = M.Map String Oraculo

{- Instancias -} 


instance Show Oraculo where
 show = init . prettyOraculo 0

prettyOraculo :: Int -> Oraculo -> String
prettyOraculo _ (Prediccion s) = s ++ "\n"
prettyOraculo n (Pregunta s m) = pregunta ++ opciones
 where
  wrap xs = '¿':xs ++ "?"

  sangria  = replicate (n+3) ' ' 
  pregunta = wrap s ++ "\n"
  opciones = concatMap opcion . M.toList $ m
   where 
    opcion (a,b) = sangria ++ '-' : a ++ ": " ++ prettyOraculo (n+3) b


-- Test basico para imprimir (NO BORRAR)
sampleOraculo_1 = Pregunta { pregunta = "Eres hombre " ,
                   opciones = M.fromList [ ("Si", Pregunta { pregunta="Te gusta el pan",
                     opciones=M.fromList [ ("Si", Prediccion {prediccion="gordo"}),
                       ("No", Prediccion "sano") ] } ),
                   ("No", Pregunta { pregunta = "Eres veneco",
                       opciones=M.fromList [ ("Si", Prediccion "tu futuro es incierto"),  
                     ("No", Prediccion "Bendecida y afortunada") ] }) ]
                  }


{- Construccion -} 

-- Creates an oracle from a String prediction
crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

{- Acceso -}
respuesta :: Oraculo -> String -> Oraculo
respuesta Pregunta{opciones=opc} s = 
    let fromJust Nothing   = error "Unexpected" 
        fromJust (Just xd) = xd
    in fromJust $ M.lookup s opc 
respuesta _                _ = error "respuesta: invalid value constructor."

{- Modificacion -}

-- creates an answer 
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opc ors preg = Pregunta preg opcs
    where opcs = M.fromList $ zip opc ors

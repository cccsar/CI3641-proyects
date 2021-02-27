module Oraculo (
  Oraculo (..) , 
  crearOraculo, 
  respuesta, 
  ramificar,
  sampleOraculo,
  sampleText
) where


import qualified Data.Map as M
import Text.ParserCombinators.ReadP
 
data Oraculo = Prediccion {prediccion :: String} 
             | Pregunta {pregunta :: String, opciones :: Opciones}

type Opciones = M.Map String Oraculo


{- Instancias -} 


instance Show Oraculo where
 show = init . prettyOraculo 0

instance Read Oraculo where 
  readsPrec _ = readP_to_S (oracleP 0) 


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



{- Funciones de ayuda -} 


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




{- Parseo -} 


-- Parsers para ¿?
openingQM,closingQM :: ReadP Char

openingQM = char '¿'
closingQM = char '?' 

-- Parser para caracteres validos
valid :: ReadP Char
valid = satisfy isQM 
 where  isQM c = c /= '?' && c /= '¿' && c /= '-' && c /= ':' && c /= '\n'


text, qP, optionP :: ReadP String

-- Parser para texto valido
text = many1 valid

-- Parser para preguntas (temporal)
qP  = between ying yang text
 where ying = many1 openingQM >> skipSpaces ; yang = skipSpaces >> many1 closingQM 

optionP = between dashP colonP text -- tentative
 where dashP   = char '-' >> skipSpaces ; colonP  = skipSpaces >> char ':'


{- Parsers para el tipo de datos en cuestion -}

questionP, predictionP, oracleP :: Int -> ReadP Oraculo

-- Parsea texto correspondiente al constructor 'Pregunta'
questionP n = do 
  p <- qP 
  count 1 (char '\n') 
  q <- mapP (n+3)
  return (Pregunta p q) 

-- Parsea texto correspondiente al constructor 'Prediccion'
predictionP _ = do 
  x <- text 
  count 1 (char '\n')
  return (Prediccion x) 

-- Parsea el mapa de opciones para el constructor 'Pregunta'
mapP :: Int -> ReadP (M.Map String Oraculo)
mapP n = M.fromList <$> many1 mapBody
 where 
  -- mapBody :: ReadP Oraculo 
  mapBody = do 
   count n (char ' ') 
   p <- optionP 
   count 1 (char ' ') 
   q <- oracleP n
   return (p,q) 

-- Parsea el tipo de datos Oraculo, a saber o una pregunta, o una prediccion
oracleP n = choice [ questionP n , predictionP n] 


{- Casos de prueba -} 

-- Test basico para leer
sampleText = "¿Eres venezolano?\n"++
             "   -si: ¿eres mayor de edad?\n"++
             "      -si: mal por ti\n"++
             "      -no: aun hay esperanza, huye\n"++
             "   -no: ¿que se siente comer bien?\n"++
             "      -es lo mejor del mundo: ¿por que lo dices?\n"++
             "         -porque si: ok\n"++
             "         -no quiero responder: ok\n"++
             "      -no es la gran cosa: no sabes lo que estas diciendo\n"

-- Test basico para imprimir (NO BORRAR)
sampleOraculo = Pregunta { pregunta = "Eres hombre" ,
                   opciones = M.fromList [ ("Si", Pregunta { pregunta="Te gusta el pan",
                     opciones=M.fromList [ ("Si", Prediccion {prediccion="gordo"}),
                       ("No", Prediccion "sano") ] } ),
                   ("No", Pregunta { pregunta = "Eres veneco",
                       opciones=M.fromList [ ("Si", Prediccion "tu futuro es incierto"),  
                     ("No", Prediccion "Bendecida y afortunada") ] }) ]
                  }

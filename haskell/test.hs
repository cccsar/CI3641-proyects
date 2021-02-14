module Test where

import Data.Char (isDigit, isSpace) 
import qualified Data.Map as M 
import Text.ParserCombinators.ReadP
import Control.Applicative (liftA2) 

data Oraculo = Prediccion String | Pregunta String (M.Map String Oraculo) 

instance Show Oraculo where
 show = init . prettyOraculo 0

prettyOraculo :: Int -> Oraculo -> String
prettyOraculo _ (Prediccion s) = s ++ "\n"
prettyOraculo n (Pregunta s m) = pregunta ++ opciones m
 where
  sangria  = replicate (n+3) ' ' 
  pregunta = wrap s ++ "\n"
  opciones = concatMap (\(a,b)-> sangria ++ '-' : a ++ ": " ++ prettyOraculo (n+3) b) .  M.toList 

  -- wrap :: Char -> Char -> String 
  wrap xs = '¿':xs ++ "?"

{-
 - between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
 - choice :: [ReadP a] -> ReadP a
 -
 - get :: ReadP Char -- skips char
 - look :: ReadP String -- skips string
 -
 - satisfy :: (Char -> Bool) -> ReadP Char
 - skipManny :: ReadP a -> ReadP ()
 -
 - char :: Char -> ReadP Char
 - string :: String -> ReadP String
 -
 - readP_to_S :: ReadP a -> ReadS a -- intercambio
 - readS_to_P :: ReadS a -> ReadP a
 -
 - many :: ReadP a -> ReadP [a]     -- parses 0 or more ocurrences of said parserr
 - choice :: [ReadP a] -> ReadP a -- combines parsers
 - skipSpaces :: ReadP ()
 -
 -}

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
qP  = between (openingQM >> skipSpaces) (skipSpaces >> closingQM) text

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

-- Pregunta string prediccion | Pregunta String (Pregunta) Prediccion

{-
 - CFG for the aimed type: 
 - T ::= any
 - E ::= '¿'any'?' F 
 - F ::= '*'any':' E F | '*'any':' T
 - any ::= characters
 -}


-- Parser tester
runParser :: String -> ReadP Oraculo -> (Oraculo,String)
runParser ss p = case readP_to_S p ss of 
  [] -> error "Failed parsing" 
  xs -> last xs


{- 
 - type ReadS a = String -> [(a,String)] 
 -
 - readsPrec :: Int -> ReadS a 
 - reads  :: ReadS a -- readsPrec 0 
 - lex :: ReadS String
 - readList :: ReadS [a]
 - readParen :: Bool -> ReadS a -> ReadS a 
 -}



-----------------------



{- LCA -} 

data Tree a = Branch a [Tree a] | Leaf a deriving (Show, Eq) 

-- For this to work all elements on the tree must be different as well as 
-- elements to search.
lca :: Eq a => a -> a -> Tree a -> Maybe a 
lca a b t = case found of 
             [a,b] -> Just . fst . last . takeWhile (\(a,b) -> a == b) . zip (reverse a) $ (reverse b)
             _     -> Nothing
 where 
  found = lookupT [] a b t  

  --lookupT :: Eq a => [a] -> a -> a -> Tree a -> [[a]] 
  lookupT z a b (Branch e xs) 
   | e == a || e == b = [ e:z ] 
   | otherwise        = concatMap (lookupT (e:z) a b) xs
  lookupT z a b (Leaf e) 
   | e == a || e == b = [ e:z ]
   | otherwise        = []
-- It works 

sampleTree = Branch 'a' 
              [ Branch 'b' 
               [ Branch 'g' 
                [ Branch 'h' 
                 [ Leaf 'j' ], 
                 Leaf 'i' ] ], 
               Branch 'c' 
                [ Branch 'k' 
                 [ Leaf 'l', 
                Branch 'm' 
                 [ Leaf 'n' ] ] ],
               Branch 'd' [ Leaf 'o' ], 
               Branch 'e' 
                [ Branch 'p' 
                 [ Leaf 'q', 
                 Branch 'r' 
                  [ Leaf 's', 
                  Branch 't' [
                   Leaf 'w' ],
                  Branch 'u' [
                   Leaf 'x'],
                  Leaf 'v' ] ] ], 
               Branch 'f' [
                Leaf 'z' ] ]

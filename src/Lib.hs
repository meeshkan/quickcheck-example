-- | This JSON package retains the order of array elements.
--   JSON: http://www.ietf.org/rfc/rfc4627.txt

module Lib (
    JSON(..)
  , parseJSON
  , dumpJSON
  ) where

import Control.Applicative ((<*),(*>),(<$>),(<$))
import Control.Monad (void)
import Data.Char
import Data.List (foldl', intercalate)
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------

parseJSON :: String -> Either ParseError JSON
parseJSON = parse json "json"

----------------------------------------------------------------

dumpJSON :: JSON -> String
dumpJSON (JSBool True) = "true"
dumpJSON (JSBool False) = "false"
dumpJSON JSNull = "null"
dumpJSON (JSNumber x) = show x
dumpJSON (JSFloat x) = show x
dumpJSON (JSString x) = intercalate "" ["\"", x, "\""]
dumpJSON (JSArray arr) = intercalate " " [
    "["
  , intercalate ", " [dumpJSON x | x <- arr]
  , "]"
  ]
dumpJSON (JSObject obj) = intercalate " " [
    "{"
  , intercalate ", " [ intercalate " " [dumpJSON $ JSString x, ":", dumpJSON y] | (x, y) <- obj]
  , "}"
  ]

----------------------------------------------------------------

data JSON = JSNull
          | JSBool Bool
          | JSNumber Int
          | JSFloat Double
          | JSString String
          | JSArray [JSON]
          | JSObject [(String,JSON)]
          deriving (Show, Eq)

----------------------------------------------------------------

json :: Parser JSON
json = ws *> jsValue

jsValue :: Parser JSON
jsValue = choice [jsNull,jsBool,jsObject,jsArray,try jsFloat,jsNumber,jsString]

----------------------------------------------------------------

-- |
--
-- >>> parseJSON "  null "
-- Right JSNull
jsNull :: Parser JSON
jsNull = jsAtom "null" JSNull

-- |
--
-- >>> parseJSON "  false "
-- Right (JSBool False)
-- >>> parseJSON "true"
-- Right (JSBool True)
jsBool :: Parser JSON
jsBool = jsAtom "false" (JSBool False)
     <|> jsAtom "true"  (JSBool True)

----------------------------------------------------------------

-- |
--
-- >>> parseJSON "  { \"key1\": 2 ,  \"key2\" : false } "
-- Right (JSObject [("key1",JSNumber 2),("key2",JSBool False)])
jsObject :: Parser JSON
jsObject = JSObject <$> betweenWs '{' kvs '}'
  where
    kvs = kv `sepBy` charWs ','
    kv = do
        JSString key <- jsString
        charWs ':'
        val <- jsValue
        return (key, val)

----------------------------------------------------------------

-- |
--
-- >>> parseJSON "  [ 1 , \"foo\" ,  true ] "
-- Right (JSArray [JSNumber 1,JSString "foo",JSBool True])
jsArray :: Parser JSON
jsArray = JSArray <$> betweenWs '[' vals ']'
  where
    vals = jsValue `sepBy` charWs ','

----------------------------------------------------------------

-- | Integer only.
--
-- >>> parseJSON "  123 "
-- Right (JSNumber 123)
-- >>> parseJSON "  -456 "
-- Right (JSNumber (-456))
jsNumber :: Parser JSON
jsNumber = JSNumber <$> do
    sign <- option id (negate <$ char '-')
    ns   <- many1 $ oneOf ['0'..'9']
    ws
    return $ sign $ fromInts ns
  where
    fromInts = foldl' (\x y -> x*10 + toInt y) 0
    toInt n = ord n - ord '0'

----------------------------------------------------------------

-- | Real only.
--
-- >>> parseJSON "  123.0 "
-- Right (JSFloat 123.0)
-- >>> parseJSON "  -456.01 "
-- Right (JSFloat (-456.01))
jsFloat :: Parser JSON
jsFloat = JSFloat <$> do
    sign <- option id (negate <$ char '-')
    ns   <- many1 $ oneOf ['0'..'9']
    void $ char '.'
    frac <- many1 $ oneOf ['0'..'9']
    ws
    return $ sign $ ((leftSide ns) + (fst (rightSide frac)))
  where
    leftSide = foldl' (\x y -> x * 10 + toFloat y) 0.0
    rightSide = foldl' (\(x,z) y -> (x + ((toFloat y) / (10.0 ** z)), z+1)) (0.0, 1)
    toFloat n = (1 :: Double) * fromIntegral (ord n - ord '0')

----------------------------------------------------------------

-- | Non Unicode only.
--
-- >>> parseJSON " \"foo bar baz\"  "
-- Right (JSString "foo bar baz")
jsString :: Parser JSON
jsString = JSString <$> (between (char '"') (char '"') (many jsChar) <* ws)
  where
    jsChar = unescaped <|> escaped
    unescaped = noneOf "\"\\"
    escaped   = char '\\' *> escapedChar

ch :: (Char, Char) -> Parser Char
ch (x, y) = y <$ char x

escapedChar :: Parser Char
escapedChar = choice $ map ch alist
  where
    alist = [
        ('b', '\b')
      , ('f', '\f')
      , ('n', '\n')
      , ('r', '\r')
      , ('t', '\t')
      , ('\\','\\')
      , ('\"','\"')
      ]

----------------------------------------------------------------

ws :: Parser ()
ws = void $ many $ oneOf " \t\r\n"

jsAtom :: String -> JSON -> Parser JSON
jsAtom str val = val <$ (string str <* ws)

charWs :: Char -> Parser ()
charWs c = char c *> ws

betweenWs :: Char -> Parser a -> Char -> Parser a
betweenWs o vals c = charWs o *> vals <* charWs c
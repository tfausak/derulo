{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Derulo parses and renders JSON simply. It aims to provide an RFC 7159
-- compliant parser and renderer without incurring any dependencies. It is
-- intended to be used either for learning or in situations where dependencies
-- are unwanted. In normal usage, prefer a faster, more robust library like
-- [Aeson](https://hackage.haskell.org/package/aeson).
--
-- Derulo does not export any identifiers that conflict with the prelude and
-- can be imported unqualified.
--
-- >>> import Derulo
--
-- Use 'readJSON' to parse a 'String' into a 'JSON' value.
--
-- >>> readJSON " null "
-- Just Null
--
-- Use 'showJSON' to render a 'JSON' value as a 'String'.
--
-- >>> showJSON Null
-- "null"
module Derulo
  ( JSON(..)
  , readJSON
  , showJSON
  ) where

import qualified Control.Monad as Monad
import qualified Data.Data as Data
import qualified Data.Functor as Functor
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified GHC.Generics as Generics
import qualified Text.ParserCombinators.ReadP as ReadP

-- * Types
-- | A JSON value as described by RFC 7159.
data JSON
  = Null
  | Boolean Bool
  | Number Integer
           Integer
  | String String
  | Array [JSON]
  | Object [(String, JSON)]
  deriving (Data.Data, Eq, Generics.Generic, Ord, Read, Show)

-- * Parsing
-- | Parses a string as JSON.
readJSON :: String -> Maybe JSON
readJSON = runParser pJSON

pJSON :: ReadP.ReadP JSON
pJSON = do
  pWhitespaces
  value <- pValue
  ReadP.eof
  pure value

pValue :: ReadP.ReadP JSON
pValue = ReadP.choice [pNull, pBoolean, pNumber, pString, pArray, pObject]

pNull :: ReadP.ReadP JSON
pNull = do
  pSymbol "null"
  pure Null

pBoolean :: ReadP.ReadP JSON
pBoolean = pTrue ReadP.+++ pFalse

pTrue :: ReadP.ReadP JSON
pTrue = do
  pSymbol "true"
  pure (Boolean True)

pFalse :: ReadP.ReadP JSON
pFalse = do
  pSymbol "false"
  pure (Boolean False)

pNumber :: ReadP.ReadP JSON
pNumber = do
  integer <- pInteger
  (fraction, precision) <- ReadP.option (0, 0) pFraction
  power <- ReadP.option 0 pPower
  pWhitespaces
  let mantissa = integer * 10 ^ precision + negateIf (integer <= 0) fraction
  let magnitude = power - precision
  pure (Number mantissa magnitude)

pInteger :: ReadP.ReadP Integer
pInteger = pZero ReadP.+++ pNonZero

pZero :: ReadP.ReadP Integer
pZero = do
  ReadP.optional (ReadP.char '-')
  Functor.void (ReadP.char '0')
  pure 0

pNonZero :: ReadP.ReadP Integer
pNonZero = do
  sign <- ReadP.option '+' (ReadP.char '-')
  first <- ReadP.satisfy isNonZeroDigit
  rest <- ReadP.munch isDecimalDigit
  case fromDecimal (first : rest) of
    Nothing -> ReadP.pfail
    Just nonZero -> pure (negateIf (sign == '-') nonZero)

pFraction :: ReadP.ReadP (Integer, Integer)
pFraction = do
  Functor.void (ReadP.char '.')
  digits <- ReadP.munch1 isDecimalDigit
  case fromDecimal digits of
    Nothing -> ReadP.pfail
    Just fraction -> pure (fraction, List.genericLength digits)

pPower :: ReadP.ReadP Integer
pPower = do
  Functor.void (ReadP.char 'E' ReadP.+++ ReadP.char 'e')
  sign <- ReadP.option '+' (ReadP.char '+' ReadP.+++ ReadP.char '-')
  digits <- ReadP.munch1 isDecimalDigit
  case fromDecimal digits of
    Nothing -> ReadP.pfail
    Just magnitude -> pure (negateIf (sign == '-') magnitude)

pString :: ReadP.ReadP JSON
pString = do
  string <- ReadP.between
    (ReadP.char '"')
    (ReadP.char '"')
    (do
      characters <- ReadP.many pCharacter
      pure (String characters)
    )
  pWhitespaces
  pure string

pCharacter :: ReadP.ReadP Char
pCharacter = pLiteral ReadP.+++ pEscape

pLiteral :: ReadP.ReadP Char
pLiteral = ReadP.satisfy isLiteral

pEscape :: ReadP.ReadP Char
pEscape = do
  Functor.void (ReadP.char '\\')
  escape <- ReadP.get
  case escape of
    '"' -> pure '"'
    '/' -> pure '/'
    '\\' -> pure '\\'
    'b' -> pure '\b'
    'f' -> pure '\f'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'u' -> do
      digits <- ReadP.count 4 (ReadP.satisfy isHexadecimalDigit)
      case fromHexadecimal digits of
        Nothing -> ReadP.pfail
        Just point -> pure (toEnum (fromIntegral point))
    _ -> ReadP.pfail

pArray :: ReadP.ReadP JSON
pArray = ReadP.between
  (pSymbol "[")
  (pSymbol "]")
  (do
    values <- ReadP.sepBy pValue (pSymbol ",")
    pure (Array values)
  )

pObject :: ReadP.ReadP JSON
pObject = ReadP.between
  (pSymbol "{")
  (pSymbol "}")
  (do
    pairs <- ReadP.sepBy pPair (pSymbol ",")
    pure (Object pairs)
  )

pPair :: ReadP.ReadP (String, JSON)
pPair = do
  String key <- pString
  pSymbol ":"
  value <- pValue
  pure (key, value)

-- * Rendering
-- | Renders JSON as a string.
showJSON :: JSON -> String
showJSON json = sJSON json ""

sJSON :: JSON -> ShowS
sJSON json = case json of
  Null -> sNull
  Boolean boolean -> sBoolean boolean
  Number mantissa magnitude -> sNumber mantissa magnitude
  String string -> sString string
  Array array -> sArray array
  Object object -> sObject object

sNull :: ShowS
sNull = showString "null"

sBoolean :: Bool -> ShowS
sBoolean boolean = if boolean then sTrue else sFalse

sTrue :: ShowS
sTrue = showString "true"

sFalse :: ShowS
sFalse = showString "false"

sNumber :: Integer -> Integer -> ShowS
sNumber mantissa magnitude = shows mantissa . showChar 'e' . shows magnitude

sString :: String -> ShowS
sString = sSeparatedBetween (showChar '"') (showChar '"') id sCharacter

sCharacter :: Char -> ShowS
sCharacter character = case character of
  '"' -> showString "\\\""
  '\\' -> showString "\\\\"
  '\b' -> showString "\\b"
  '\f' -> showString "\\f"
  '\n' -> showString "\\n"
  '\r' -> showString "\\r"
  '\t' -> showString "\\t"
  _ -> if isControl character
    then showString "\\u" . showString
      (padLeft 4 '0' (toHexadecimal (fromIntegral (fromEnum character))))
    else showChar character

sArray :: [JSON] -> ShowS
sArray = sSeparatedBetween (showChar '[') (showChar ']') (showChar ',') sJSON

sObject :: [(String, JSON)] -> ShowS
sObject = sSeparatedBetween (showChar '{') (showChar '}') (showChar ',') sPair

sPair :: (String, JSON) -> ShowS
sPair (key, value) = sString key . showChar ':' . sJSON value

-- * Helpers
fromBase :: Integer -> (Char -> Maybe Integer) -> String -> Maybe Integer
fromBase b f = Monad.foldM
  (\n c -> do
    d <- f c
    pure (b * n + d)
  )
  0

fromDecimal :: String -> Maybe Integer
fromDecimal = fromBase 10 fromDecimalDigit

fromDecimalDigit :: Char -> Maybe Integer
fromDecimalDigit c = case c of
  '0' -> Just 0
  '1' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  _ -> Nothing

fromHexadecimal :: String -> Maybe Integer
fromHexadecimal = fromBase 16 fromHexadecimalDigit

fromHexadecimalDigit :: Char -> Maybe Integer
fromHexadecimalDigit c = case c of
  'A' -> Just 10
  'B' -> Just 11
  'C' -> Just 12
  'D' -> Just 13
  'E' -> Just 14
  'F' -> Just 15
  'a' -> Just 10
  'b' -> Just 11
  'c' -> Just 12
  'd' -> Just 13
  'e' -> Just 14
  'f' -> Just 15
  _ -> fromDecimalDigit c

isControl :: Char -> Bool
isControl c = '\x00' <= c && c <= '\x1f'

isDecimalDigit :: Char -> Bool
isDecimalDigit c = '0' <= c && c <= '9'

isHexadecimalDigit :: Char -> Bool
isHexadecimalDigit c =
  isDecimalDigit c || 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f'

isLiteral :: Char -> Bool
isLiteral c = not (c == '"' || c == '\\' || isControl c)

isNonZeroDigit :: Char -> Bool
isNonZeroDigit c = '1' <= c && c <= '9'

isWhitespace :: Char -> Bool
isWhitespace c = c == '\t' || c == '\n' || c == '\r' || c == ' '

negateIf :: Bool -> Integer -> Integer
negateIf p n = if p then negate n else n

pSymbol :: String -> ReadP.ReadP ()
pSymbol s = do
  Functor.void (ReadP.string s)
  pWhitespaces

pWhitespaces :: ReadP.ReadP ()
pWhitespaces = Functor.void (ReadP.munch isWhitespace)

padLeft :: Integer -> a -> [a] -> [a]
padLeft n x ys = reverse (padRight n x (reverse ys))

padRight :: Integer -> a -> [a] -> [a]
padRight n x ys = if n <= 0
  then ys
  else case ys of
    [] -> x : padRight (n - 1) x ys
    y : zs -> y : padRight (n - 1) x zs

runParser :: ReadP.ReadP a -> String -> Maybe a
runParser p s = Maybe.listToMaybe
  (Maybe.mapMaybe
    (\(x, t) -> if null t then Just x else Nothing)
    (ReadP.readP_to_S p s)
  )

sBetween :: ShowS -> ShowS -> (anything -> ShowS) -> anything -> ShowS
sBetween left right render it = left . render it . right

sSeparated :: ShowS -> (element -> ShowS) -> [element] -> ShowS
sSeparated separator render elements = case elements of
  [] -> id
  [element] -> render element
  element : rest ->
    render element . separator . sSeparated separator render rest

sSeparatedBetween
  :: ShowS -> ShowS -> ShowS -> (element -> ShowS) -> [element] -> ShowS
sSeparatedBetween left right separator render =
  sBetween left right (sSeparated separator render)

toBase :: Integer -> (Integer -> Maybe Char) -> Integer -> String
toBase b f n =
  if n == 0 then [Maybe.fromJust (f n)] else reverse (toBase' b f n)

toBase' :: Integer -> (Integer -> Maybe Char) -> Integer -> String
toBase' b f n = case quotRem n b of
  (0, 0) -> ""
  (q, r) -> Maybe.fromJust (f r) : toBase' b f q

toHexadecimal :: Integer -> String
toHexadecimal = toBase 16 toHexadecimalDigit

toHexadecimalDigit :: Integer -> Maybe Char
toHexadecimalDigit n = case n of
  0 -> Just '0'
  1 -> Just '1'
  2 -> Just '2'
  3 -> Just '3'
  4 -> Just '4'
  5 -> Just '5'
  6 -> Just '6'
  7 -> Just '7'
  8 -> Just '8'
  9 -> Just '9'
  10 -> Just 'a'
  11 -> Just 'b'
  12 -> Just 'c'
  13 -> Just 'd'
  14 -> Just 'e'
  15 -> Just 'f'
  _ -> Nothing

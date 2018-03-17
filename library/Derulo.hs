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
--
-- >>> readJSON "null"
-- Just Null
--
-- >>> readJSON "true"
-- Just (Boolean True)
-- >>> readJSON "false"
-- Just (Boolean False)
--
-- >>> readJSON "0e0"
-- Just (Number 0 0)
-- >>> readJSON "12e34"
-- Just (Number 12 34)
-- >>> readJSON "-12e-34"
-- Just (Number (-12) (-34))
--
-- >>> readJSON "\"\""
-- Just (String "")
-- >>> readJSON "\"js\""
-- Just (String "js")
-- >>> readJSON "\"\\\"\\\\\\b\\f\\n\\r\\t\""
-- Just (String "\"\\\b\f\n\r\t")
-- >>> readJSON "\"\\u001f\""
-- Just (String "\US")
--
-- >>> readJSON "[]"
-- Just (Array [])
-- >>> readJSON "[null]"
-- Just (Array [Null])
-- >>> readJSON "[true,false]"
-- Just (Array [Boolean True,Boolean False])
--
-- >>> readJSON "{}"
-- Just (Object [])
-- >>> readJSON "{\"\":null}"
-- Just (Object [("",Null)])
-- >>> readJSON "{\"t\":true,\"f\":false}"
-- Just (Object [("t",Boolean True),("f",Boolean False)])
readJSON :: String -> Maybe JSON
readJSON string = runParser pJSON string

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
  string <-
    ReadP.between
      (ReadP.char '"')
      (ReadP.char '"')
      (do characters <- ReadP.many pCharacter
          pure (String characters))
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
pArray =
  ReadP.between
    (pSymbol "[")
    (pSymbol "]")
    (do values <- ReadP.sepBy pValue (pSymbol ",")
        pure (Array values))

pObject :: ReadP.ReadP JSON
pObject =
  ReadP.between
    (pSymbol "{")
    (pSymbol "}")
    (do pairs <- ReadP.sepBy pPair (pSymbol ",")
        pure (Object pairs))

pPair :: ReadP.ReadP (String, JSON)
pPair = do
  String key <- pString
  pSymbol ":"
  value <- pValue
  pure (key, value)

-- * Rendering
-- | Renders JSON as a string.
--
-- >>> showJSON Null
-- "null"
--
-- >>> showJSON (Boolean True)
-- "true"
-- >>> showJSON (Boolean False)
-- "false"
--
-- >>> showJSON (Number 0 0)
-- "0e0"
-- >>> showJSON (Number 12 34)
-- "12e34"
-- >>> showJSON (Number (-12) (-34))
-- "-12e-34"
--
-- >>> showJSON (String "")
-- "\"\""
-- >>> showJSON (String "js")
-- "\"js\""
-- >>> showJSON (String "\"\\\b\f\n\r\t")
-- "\"\\\"\\\\\\b\\f\\n\\r\\t\""
-- >>> showJSON (String "\x1f")
-- "\"\\u001f\""
--
-- >>> showJSON (Array [])
-- "[]"
-- >>> showJSON (Array [Null])
-- "[null]"
-- >>> showJSON (Array [Boolean True, Boolean False])
-- "[true,false]"
--
-- >>> showJSON (Object [])
-- "{}"
-- >>> showJSON (Object [("", Null)])
-- "{\"\":null}"
-- >>> showJSON (Object [("t", Boolean True), ("f", Boolean False)])
-- "{\"t\":true,\"f\":false}"
showJSON :: JSON -> String
showJSON json = sJSON json ""

sJSON :: JSON -> ShowS
sJSON json =
  case json of
    Null -> sNull
    Boolean boolean -> sBoolean boolean
    Number mantissa magnitude -> sNumber mantissa magnitude
    String string -> sString string
    Array array -> sArray array
    Object object -> sObject object

sNull :: ShowS
sNull = showString "null"

sBoolean :: Bool -> ShowS
sBoolean boolean =
  if boolean
    then sTrue
    else sFalse

sTrue :: ShowS
sTrue = showString "true"

sFalse :: ShowS
sFalse = showString "false"

sNumber :: Integer -> Integer -> ShowS
sNumber mantissa magnitude = shows mantissa . showChar 'e' . shows magnitude

sString :: String -> ShowS
sString string =
  sSeparatedBetween (showChar '"') (showChar '"') id sCharacter string

sCharacter :: Char -> ShowS
sCharacter character =
  case character of
    '"' -> showString "\\\""
    '\\' -> showString "\\\\"
    '\b' -> showString "\\b"
    '\f' -> showString "\\f"
    '\n' -> showString "\\n"
    '\r' -> showString "\\r"
    '\t' -> showString "\\t"
    _ ->
      if isControl character
        then showString "\\u" .
             showString
               (padLeft
                  4
                  '0'
                  (toHexadecimal (fromIntegral (fromEnum character))))
        else showChar character

sArray :: [JSON] -> ShowS
sArray array =
  sSeparatedBetween (showChar '[') (showChar ']') (showChar ',') sJSON array

sObject :: [(String, JSON)] -> ShowS
sObject object =
  sSeparatedBetween (showChar '{') (showChar '}') (showChar ',') sPair object

sPair :: (String, JSON) -> ShowS
sPair (key, value) = sString key . showChar ':' . sJSON value

-- * Helpers
fromBase :: Integer -> (Char -> Maybe Integer) -> String -> Maybe Integer
fromBase b f s =
  Monad.foldM
    (\n c -> do
       d <- f c
       pure (b * n + d))
    0
    s

fromDecimal :: String -> Maybe Integer
fromDecimal s = fromBase 10 fromDecimalDigit s

fromDecimalDigit :: Char -> Maybe Integer
fromDecimalDigit c =
  case c of
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
fromHexadecimal s = fromBase 16 fromHexadecimalDigit s

fromHexadecimalDigit :: Char -> Maybe Integer
fromHexadecimalDigit c =
  case c of
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
negateIf p n =
  if p
    then negate n
    else n

pSymbol :: String -> ReadP.ReadP ()
pSymbol s = do
  Functor.void (ReadP.string s)
  pWhitespaces

pWhitespaces :: ReadP.ReadP ()
pWhitespaces = Functor.void (ReadP.munch isWhitespace)

padLeft :: Integer -> a -> [a] -> [a]
padLeft n x ys = reverse (padRight n x (reverse ys))

padRight :: Integer -> a -> [a] -> [a]
padRight n x ys =
  if n <= 0
    then ys
    else case ys of
           [] -> x : padRight (n - 1) x ys
           y:zs -> y : padRight (n - 1) x zs

runParser :: ReadP.ReadP a -> String -> Maybe a
runParser p s =
  Maybe.listToMaybe
    (Maybe.mapMaybe
       (\(x, t) ->
          if null t
            then Just x
            else Nothing)
       (ReadP.readP_to_S p s))

sBetween :: ShowS -> ShowS -> (anything -> ShowS) -> anything -> ShowS
sBetween left right render it = left . render it . right

sSeparated :: ShowS -> (element -> ShowS) -> [element] -> ShowS
sSeparated separator render elements =
  case elements of
    [] -> id
    [element] -> render element
    element:rest ->
      render element . separator . sSeparated separator render rest

sSeparatedBetween ::
     ShowS -> ShowS -> ShowS -> (element -> ShowS) -> [element] -> ShowS
sSeparatedBetween left right separator render elements =
  sBetween left right (sSeparated separator render) elements

toBase :: Integer -> (Integer -> Maybe Char) -> Integer -> String
toBase b f n =
  if n == 0
    then [Maybe.fromJust (f n)]
    else reverse (toBase' b f n)

toBase' :: Integer -> (Integer -> Maybe Char) -> Integer -> String
toBase' b f n =
  case quotRem n b of
    (0, 0) -> ""
    (q, r) -> Maybe.fromJust (f r) : toBase' b f q

toHexadecimal :: Integer -> String
toHexadecimal n = toBase 16 toHexadecimalDigit n

toHexadecimalDigit :: Integer -> Maybe Char
toHexadecimalDigit n =
  case n of
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

-- | Tests taken from <https://github.com/nst/JSONTestSuite>.
--
-- >>> readJSON "[123.456e-789]" -- i_number_double_huge_neg_exp.json
-- Just (Array [Number 123456 (-792)])
-- >>> readJSON "[0.4e00669999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999969999999006]" -- i_number_huge_exp.json
-- Just (Array [Number (-4) 669999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999969999999005])
-- >>> readJSON "[-1e+9999]" -- i_number_neg_int_huge_exp.json
-- Just (Array [Number (-1) 9999])
-- >>> readJSON "[1.5e+9999]" -- i_number_pos_double_huge_exp.json
-- Just (Array [Number 15 9998])
-- >>> readJSON "[-123123e100000]" -- i_number_real_neg_overflow.json
-- Just (Array [Number (-123123) 100000])
-- >>> readJSON "[123123e100000]" -- i_number_real_pos_overflow.json
-- Just (Array [Number 123123 100000])
-- >>> readJSON "[123e-10000000]" -- i_number_real_underflow.json
-- Just (Array [Number 123 (-10000000)])
-- >>> readJSON "[-123123123123123123123123123123]" -- i_number_too_big_neg_int.json
-- Just (Array [Number (-123123123123123123123123123123) 0])
-- >>> readJSON "[100000000000000000000]" -- i_number_too_big_pos_int.json
-- Just (Array [Number 100000000000000000000 0])
-- >>> readJSON "[-237462374673276894279832749832423479823246327846]" -- i_number_very_big_negative_int.json
-- Just (Array [Number (-237462374673276894279832749832423479823246327846) 0])
-- >>> readJSON "{\"\\uDFAA\":0}" -- i_object_key_lone_2nd_surrogate.json
-- Just (Object [("\57258",Number 0 0)])
-- >>> readJSON "[\"\\uDADA\"]" -- i_string_1st_surrogate_but_2nd_missing.json
-- Just (Array [String "\56026"])
-- >>> readJSON "[\"\\uD888\\u1234\"]" -- i_string_1st_valid_surrogate_2nd_invalid.json
-- Just (Array [String "\55432\4660"])
-- >>> readJSON "[\"\\uD800\\n\"]" -- i_string_incomplete_surrogate_and_escape_valid.json
-- Just (Array [String "\55296\n"])
-- >>> readJSON "[\"\\uDd1ea\"]" -- i_string_incomplete_surrogate_pair.json
-- Just (Array [String "\56606a"])
-- >>> readJSON "[\"\\uD800\\uD800\\n\"]" -- i_string_incomplete_surrogates_escape_valid.json
-- Just (Array [String "\55296\55296\n"])
-- >>> readJSON "[\"\\ud800\"]" -- i_string_invalid_lonely_surrogate.json
-- Just (Array [String "\55296"])
-- >>> readJSON "[\"\\ud800abc\"]" -- i_string_invalid_surrogate.json
-- Just (Array [String "\55296abc"])
-- >>> readJSON "[\"\\uDd1e\\uD834\"]" -- i_string_inverted_surrogates_U+1D11E.json
-- Just (Array [String "\56606\55348"])
-- >>> readJSON "[\"\\uDFAA\"]" -- i_string_lone_second_surrogate.json
-- Just (Array [String "\57258"])
-- >>> readJSON "\65279{}" -- i_structure_UTF-8_BOM_empty_object.json
-- Nothing
-- >>> readJSON "[1 true]" -- n_array_1_true_without_comma.json
-- Nothing
-- >>> readJSON "[\"\": 1]" -- n_array_colon_instead_of_comma.json
-- Nothing
-- >>> readJSON "[\"\"]," -- n_array_comma_after_close.json
-- Nothing
-- >>> readJSON "[,1]" -- n_array_comma_and_number.json
-- Nothing
-- >>> readJSON "[1,,2]" -- n_array_double_comma.json
-- Nothing
-- >>> readJSON "[\"x\",,]" -- n_array_double_extra_comma.json
-- Nothing
-- >>> readJSON "[\"x\"]]" -- n_array_extra_close.json
-- Nothing
-- >>> readJSON "[\"\",]" -- n_array_extra_comma.json
-- Nothing
-- >>> readJSON "[\"x\"" -- n_array_incomplete.json
-- Nothing
-- >>> readJSON "[x" -- n_array_incomplete_invalid_value.json
-- Nothing
-- >>> readJSON "[3[4]]" -- n_array_inner_array_no_comma.json
-- Nothing
-- >>> readJSON "[1:2]" -- n_array_items_separated_by_semicolon.json
-- Nothing
-- >>> readJSON "[,]" -- n_array_just_comma.json
-- Nothing
-- >>> readJSON "[-]" -- n_array_just_minus.json
-- Nothing
-- >>> readJSON "[   , \"\"]" -- n_array_missing_value.json
-- Nothing
-- >>> readJSON "[\"a\",\n4\n,1," -- n_array_newlines_unclosed.json
-- Nothing
-- >>> readJSON "[1,]" -- n_array_number_and_comma.json
-- Nothing
-- >>> readJSON "[1,,]" -- n_array_number_and_several_commas.json
-- Nothing
-- >>> readJSON "[\"\va\"\\f]" -- n_array_spaces_vertical_tab_formfeed.json
-- Nothing
-- >>> readJSON "[*]" -- n_array_star_inside.json
-- Nothing
-- >>> readJSON "[\"\"" -- n_array_unclosed.json
-- Nothing
-- >>> readJSON "[1," -- n_array_unclosed_trailing_comma.json
-- Nothing
-- >>> readJSON "[1,\n1\n,1" -- n_array_unclosed_with_new_lines.json
-- Nothing
-- >>> readJSON "[{}" -- n_array_unclosed_with_object_inside.json
-- Nothing
-- >>> readJSON "[fals]" -- n_incomplete_false.json
-- Nothing
-- >>> readJSON "[nul]" -- n_incomplete_null.json
-- Nothing
-- >>> readJSON "[tru]" -- n_incomplete_true.json
-- Nothing
-- >>> readJSON "123\NUL" -- n_multidigit_number_then_00.json
-- Nothing
-- >>> readJSON "[++1234]" -- n_number_++.json
-- Nothing
-- >>> readJSON "[+1]" -- n_number_+1.json
-- Nothing
-- >>> readJSON "[+Inf]" -- n_number_+Inf.json
-- Nothing
-- >>> readJSON "[-01]" -- n_number_-01.json
-- Nothing
-- >>> readJSON "[-1.0.]" -- n_number_-1.0..json
-- Nothing
-- >>> readJSON "[-2.]" -- n_number_-2..json
-- Nothing
-- >>> readJSON "[-NaN]" -- n_number_-NaN.json
-- Nothing
-- >>> readJSON "[.-1]" -- n_number_.-1.json
-- Nothing
-- >>> readJSON "[.2e-3]" -- n_number_.2e-3.json
-- Nothing
-- >>> readJSON "[0.1.2]" -- n_number_0.1.2.json
-- Nothing
-- >>> readJSON "[0.3e+]" -- n_number_0.3e+.json
-- Nothing
-- >>> readJSON "[0.3e]" -- n_number_0.3e.json
-- Nothing
-- >>> readJSON "[0.e1]" -- n_number_0.e1.json
-- Nothing
-- >>> readJSON "[0E+]" -- n_number_0_capital_E+.json
-- Nothing
-- >>> readJSON "[0E]" -- n_number_0_capital_E.json
-- Nothing
-- >>> readJSON "[0e+]" -- n_number_0e+.json
-- Nothing
-- >>> readJSON "[0e]" -- n_number_0e.json
-- Nothing
-- >>> readJSON "[1.0e+]" -- n_number_1.0e+.json
-- Nothing
-- >>> readJSON "[1.0e-]" -- n_number_1.0e-.json
-- Nothing
-- >>> readJSON "[1.0e]" -- n_number_1.0e.json
-- Nothing
-- >>> readJSON "[1 000.0]" -- n_number_1_000.json
-- Nothing
-- >>> readJSON "[1eE2]" -- n_number_1eE2.json
-- Nothing
-- >>> readJSON "[2.e+3]" -- n_number_2.e+3.json
-- Nothing
-- >>> readJSON "[2.e-3]" -- n_number_2.e-3.json
-- Nothing
-- >>> readJSON "[2.e3]" -- n_number_2.e3.json
-- Nothing
-- >>> readJSON "[9.e+]" -- n_number_9.e+.json
-- Nothing
-- >>> readJSON "[Inf]" -- n_number_Inf.json
-- Nothing
-- >>> readJSON "[NaN]" -- n_number_NaN.json
-- Nothing
-- >>> readJSON "[\65297]" -- n_number_U+FF11_fullwidth_digit_one.json
-- Nothing
-- >>> readJSON "[1+2]" -- n_number_expression.json
-- Nothing
-- >>> readJSON "[0x1]" -- n_number_hex_1_digit.json
-- Nothing
-- >>> readJSON "[0x42]" -- n_number_hex_2_digits.json
-- Nothing
-- >>> readJSON "[Infinity]" -- n_number_infinity.json
-- Nothing
-- >>> readJSON "[0e+-1]" -- n_number_invalid+-.json
-- Nothing
-- >>> readJSON "[-123.123foo]" -- n_number_invalid-negative-real.json
-- Nothing
-- >>> readJSON "[-Infinity]" -- n_number_minus_infinity.json
-- Nothing
-- >>> readJSON "[-foo]" -- n_number_minus_sign_with_trailing_garbage.json
-- Nothing
-- >>> readJSON "[- 1]" -- n_number_minus_space_1.json
-- Nothing
-- >>> readJSON "[-012]" -- n_number_neg_int_starting_with_zero.json
-- Nothing
-- >>> readJSON "[-.123]" -- n_number_neg_real_without_int_part.json
-- Nothing
-- >>> readJSON "[-1x]" -- n_number_neg_with_garbage_at_end.json
-- Nothing
-- >>> readJSON "[1ea]" -- n_number_real_garbage_after_e.json
-- Nothing
-- >>> readJSON "[1.]" -- n_number_real_without_fractional_part.json
-- Nothing
-- >>> readJSON "[.123]" -- n_number_starting_with_dot.json
-- Nothing
-- >>> readJSON "[1.2a-3]" -- n_number_with_alpha.json
-- Nothing
-- >>> readJSON "[1.8011670033376514H-308]" -- n_number_with_alpha_char.json
-- Nothing
-- >>> readJSON "[012]" -- n_number_with_leading_zero.json
-- Nothing
-- >>> readJSON "[\"x\", truth]" -- n_object_bad_value.json
-- Nothing
-- >>> readJSON "{[: \"x\"}\n" -- n_object_bracket_key.json
-- Nothing
-- >>> readJSON "{\"x\", null}" -- n_object_comma_instead_of_colon.json
-- Nothing
-- >>> readJSON "{\"x\"::\"b\"}" -- n_object_double_colon.json
-- Nothing
-- >>> readJSON "{\127464\127469}" -- n_object_emoji.json
-- Nothing
-- >>> readJSON "{\"a\":\"a\" 123}" -- n_object_garbage_at_end.json
-- Nothing
-- >>> readJSON "{key: 'value'}" -- n_object_key_with_single_quotes.json
-- Nothing
-- >>> readJSON "{\"a\" b}" -- n_object_missing_colon.json
-- Nothing
-- >>> readJSON "{:\"b\"}" -- n_object_missing_key.json
-- Nothing
-- >>> readJSON "{\"a\" \"b\"}" -- n_object_missing_semicolon.json
-- Nothing
-- >>> readJSON "{\"a\":" -- n_object_missing_value.json
-- Nothing
-- >>> readJSON "{\"a\"" -- n_object_no-colon.json
-- Nothing
-- >>> readJSON "{1:1}" -- n_object_non_string_key.json
-- Nothing
-- >>> readJSON "{9999E9999:1}" -- n_object_non_string_key_but_huge_number_instead.json
-- Nothing
-- >>> readJSON "{null:null,null:null}" -- n_object_repeated_null_null.json
-- Nothing
-- >>> readJSON "{\"id\":0,,,,,}" -- n_object_several_trailing_commas.json
-- Nothing
-- >>> readJSON "{'a':0}" -- n_object_single_quote.json
-- Nothing
-- >>> readJSON "{\"id\":0,}" -- n_object_trailing_comma.json
-- Nothing
-- >>> readJSON "{\"a\":\"b\"}/**/" -- n_object_trailing_comment.json
-- Nothing
-- >>> readJSON "{\"a\":\"b\"}/**//" -- n_object_trailing_comment_open.json
-- Nothing
-- >>> readJSON "{\"a\":\"b\"}//" -- n_object_trailing_comment_slash_open.json
-- Nothing
-- >>> readJSON "{\"a\":\"b\"}/" -- n_object_trailing_comment_slash_open_incomplete.json
-- Nothing
-- >>> readJSON "{\"a\":\"b\",,\"c\":\"d\"}" -- n_object_two_commas_in_a_row.json
-- Nothing
-- >>> readJSON "{a: \"b\"}" -- n_object_unquoted_key.json
-- Nothing
-- >>> readJSON "{\"a\":\"a" -- n_object_unterminated-value.json
-- Nothing
-- >>> readJSON "{ \"foo\" : \"bar\", \"a\" }" -- n_object_with_single_string.json
-- Nothing
-- >>> readJSON "{\"a\":\"b\"}#" -- n_object_with_trailing_garbage.json
-- Nothing
-- >>> readJSON " " -- n_single_space.json
-- Nothing
-- >>> readJSON "[\"\\uD800\\\"]" -- n_string_1_surrogate_then_escape.json
-- Nothing
-- >>> readJSON "[\"\\uD800\\u\"]" -- n_string_1_surrogate_then_escape_u.json
-- Nothing
-- >>> readJSON "[\"\\uD800\\u1\"]" -- n_string_1_surrogate_then_escape_u1.json
-- Nothing
-- >>> readJSON "[\"\\uD800\\u1x\"]" -- n_string_1_surrogate_then_escape_u1x.json
-- Nothing
-- >>> readJSON "[\233]" -- n_string_accentuated_char_no_quotes.json
-- Nothing
-- >>> readJSON "[\"\\\NUL\"]" -- n_string_backslash_00.json
-- Nothing
-- >>> readJSON "[\"\\x00\"]" -- n_string_escape_x.json
-- Nothing
-- >>> readJSON "[\"\\\\\\\"]" -- n_string_escaped_backslash_bad.json
-- Nothing
-- >>> readJSON "[\"\\\t\"]" -- n_string_escaped_ctrl_char_tab.json
-- Nothing
-- >>> readJSON "[\"\\\127744\"]" -- n_string_escaped_emoji.json
-- Nothing
-- >>> readJSON "[\"\\\"]" -- n_string_incomplete_escape.json
-- Nothing
-- >>> readJSON "[\"\\u00A\"]" -- n_string_incomplete_escaped_character.json
-- Nothing
-- >>> readJSON "[\"\\uD834\\uDd\"]" -- n_string_incomplete_surrogate.json
-- Nothing
-- >>> readJSON "[\"\\uD800\\uD800\\x\"]" -- n_string_incomplete_surrogate_escape_invalid.json
-- Nothing
-- >>> readJSON "[\"\\a\"]" -- n_string_invalid_backslash_esc.json
-- Nothing
-- >>> readJSON "[\"\\uqqqq\"]" -- n_string_invalid_unicode_escape.json
-- Nothing
-- >>> readJSON "[\\u0020\"asd\"]" -- n_string_leading_uescaped_thinspace.json
-- Nothing
-- >>> readJSON "[\\n]" -- n_string_no_quotes_with_bad_escape.json
-- Nothing
-- >>> readJSON "\"" -- n_string_single_doublequote.json
-- Nothing
-- >>> readJSON "['single quote']" -- n_string_single_quote.json
-- Nothing
-- >>> readJSON "abc" -- n_string_single_string_no_double_quotes.json
-- Nothing
-- >>> readJSON "[\"\\" -- n_string_start_escape_unclosed.json
-- Nothing
-- >>> readJSON "[\"a\NULa\"]" -- n_string_unescaped_crtl_char.json
-- Nothing
-- >>> readJSON "[\"new\nline\"]" -- n_string_unescaped_newline.json
-- Nothing
-- >>> readJSON "[\"\t\"]" -- n_string_unescaped_tab.json
-- Nothing
-- >>> readJSON "\"\\UA66D\"" -- n_string_unicode_CapitalU.json
-- Nothing
-- >>> readJSON "\"\"x" -- n_string_with_trailing_garbage.json
-- Nothing
-- >>> readJSON "[\8288]" -- n_structure_U+2060_word_joined.json
-- Nothing
-- >>> readJSON "\65279" -- n_structure_UTF8_BOM_no_data.json
-- Nothing
-- >>> readJSON "<.>" -- n_structure_angle_bracket_..json
-- Nothing
-- >>> readJSON "[<null>]" -- n_structure_angle_bracket_null.json
-- Nothing
-- >>> readJSON "[1]x" -- n_structure_array_trailing_garbage.json
-- Nothing
-- >>> readJSON "[1]]" -- n_structure_array_with_extra_array_close.json
-- Nothing
-- >>> readJSON "[\"asd]" -- n_structure_array_with_unclosed_string.json
-- Nothing
-- >>> readJSON "a\229" -- n_structure_ascii-unicode-identifier.json
-- Nothing
-- >>> readJSON "[True]" -- n_structure_capitalized_True.json
-- Nothing
-- >>> readJSON "1]" -- n_structure_close_unopened_array.json
-- Nothing
-- >>> readJSON "{\"x\": true," -- n_structure_comma_instead_of_closing_brace.json
-- Nothing
-- >>> readJSON "[][]" -- n_structure_double_array.json
-- Nothing
-- >>> readJSON "]" -- n_structure_end_array.json
-- Nothing
-- >>> readJSON "[" -- n_structure_lone-open-bracket.json
-- Nothing
-- >>> readJSON "" -- n_structure_no_data.json
-- Nothing
-- >>> readJSON "[\NUL]" -- n_structure_null-byte-outside-string.json
-- Nothing
-- >>> readJSON "2@" -- n_structure_number_with_trailing_garbage.json
-- Nothing
-- >>> readJSON "{}}" -- n_structure_object_followed_by_closing_object.json
-- Nothing
-- >>> readJSON "{\"\":" -- n_structure_object_unclosed_no_value.json
-- Nothing
-- >>> readJSON "{\"a\":/*comment*/\"b\"}" -- n_structure_object_with_comment.json
-- Nothing
-- >>> readJSON "{\"a\": true} \"x\"" -- n_structure_object_with_trailing_garbage.json
-- Nothing
-- >>> readJSON "['" -- n_structure_open_array_apostrophe.json
-- Nothing
-- >>> readJSON "[," -- n_structure_open_array_comma.json
-- Nothing
-- >>> readJSON "[{" -- n_structure_open_array_open_object.json
-- Nothing
-- >>> readJSON "[\"a" -- n_structure_open_array_open_string.json
-- Nothing
-- >>> readJSON "[\"a\"" -- n_structure_open_array_string.json
-- Nothing
-- >>> readJSON "{" -- n_structure_open_object.json
-- Nothing
-- >>> readJSON "{]" -- n_structure_open_object_close_array.json
-- Nothing
-- >>> readJSON "{," -- n_structure_open_object_comma.json
-- Nothing
-- >>> readJSON "{[" -- n_structure_open_object_open_array.json
-- Nothing
-- >>> readJSON "{\"a" -- n_structure_open_object_open_string.json
-- Nothing
-- >>> readJSON "{'a'" -- n_structure_open_object_string_with_apostrophes.json
-- Nothing
-- >>> readJSON "[\"\\{[\"\\{[\"\\{[\"\\{" -- n_structure_open_open.json
-- Nothing
-- >>> readJSON "*" -- n_structure_single_star.json
-- Nothing
-- >>> readJSON "{\"a\":\"b\"}#{}" -- n_structure_trailing_#.json
-- Nothing
-- >>> readJSON "[\\u000A\"\"]" -- n_structure_uescaped_LF_before_string.json
-- Nothing
-- >>> readJSON "[1" -- n_structure_unclosed_array.json
-- Nothing
-- >>> readJSON "[ false, nul" -- n_structure_unclosed_array_partial_null.json
-- Nothing
-- >>> readJSON "[ true, fals" -- n_structure_unclosed_array_unfinished_false.json
-- Nothing
-- >>> readJSON "[ false, tru" -- n_structure_unclosed_array_unfinished_true.json
-- Nothing
-- >>> readJSON "{\"asd\":\"asd\"" -- n_structure_unclosed_object.json
-- Nothing
-- >>> readJSON "\229" -- n_structure_unicode-identifier.json
-- Nothing
-- >>> readJSON "[\8288]" -- n_structure_whitespace_U+2060_word_joiner.json
-- Nothing
-- >>> readJSON "[\f]" -- n_structure_whitespace_formfeed.json
-- Nothing
-- >>> readJSON "[[]   ]" -- y_array_arraysWithSpaces.json
-- Just (Array [Array []])
-- >>> readJSON "[\"\"]" -- y_array_empty-string.json
-- Just (Array [String ""])
-- >>> readJSON "[]" -- y_array_empty.json
-- Just (Array [])
-- >>> readJSON "[\"a\"]" -- y_array_ending_with_newline.json
-- Just (Array [String "a"])
-- >>> readJSON "[false]" -- y_array_false.json
-- Just (Array [Boolean False])
-- >>> readJSON "[null, 1, \"1\", {}]" -- y_array_heterogeneous.json
-- Just (Array [Null,Number 1 0,String "1",Object []])
-- >>> readJSON "[null]" -- y_array_null.json
-- Just (Array [Null])
-- >>> readJSON "[1\n]" -- y_array_with_1_and_newline.json
-- Just (Array [Number 1 0])
-- >>> readJSON " [1]" -- y_array_with_leading_space.json
-- Just (Array [Number 1 0])
-- >>> readJSON "[1,null,null,null,2]" -- y_array_with_several_null.json
-- Just (Array [Number 1 0,Null,Null,Null,Number 2 0])
-- >>> readJSON "[2] " -- y_array_with_trailing_space.json
-- Just (Array [Number 2 0])
-- >>> readJSON "[123e65]" -- y_number.json
-- Just (Array [Number 123 65])
-- >>> readJSON "[0e+1]" -- y_number_0e+1.json
-- Just (Array [Number 0 1])
-- >>> readJSON "[0e1]" -- y_number_0e1.json
-- Just (Array [Number 0 1])
-- >>> readJSON "[ 4]" -- y_number_after_space.json
-- Just (Array [Number 4 0])
-- >>> readJSON "[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]\n" -- y_number_double_close_to_zero.json
-- Just (Array [Number (-1) (-78)])
-- >>> readJSON "[20e1]" -- y_number_int_with_exp.json
-- Just (Array [Number 20 1])
-- >>> readJSON "[-0]" -- y_number_minus_zero.json
-- Just (Array [Number 0 0])
-- >>> readJSON "[-123]" -- y_number_negative_int.json
-- Just (Array [Number (-123) 0])
-- >>> readJSON "[-1]" -- y_number_negative_one.json
-- Just (Array [Number (-1) 0])
-- >>> readJSON "[-0]" -- y_number_negative_zero.json
-- Just (Array [Number 0 0])
-- >>> readJSON "[1E22]" -- y_number_real_capital_e.json
-- Just (Array [Number 1 22])
-- >>> readJSON "[1E-2]" -- y_number_real_capital_e_neg_exp.json
-- Just (Array [Number 1 (-2)])
-- >>> readJSON "[1E+2]" -- y_number_real_capital_e_pos_exp.json
-- Just (Array [Number 1 2])
-- >>> readJSON "[123e45]" -- y_number_real_exponent.json
-- Just (Array [Number 123 45])
-- >>> readJSON "[123.456e78]" -- y_number_real_fraction_exponent.json
-- Just (Array [Number 123456 75])
-- >>> readJSON "[1e-2]" -- y_number_real_neg_exp.json
-- Just (Array [Number 1 (-2)])
-- >>> readJSON "[1e+2]" -- y_number_real_pos_exponent.json
-- Just (Array [Number 1 2])
-- >>> readJSON "[123]" -- y_number_simple_int.json
-- Just (Array [Number 123 0])
-- >>> readJSON "[123.456789]" -- y_number_simple_real.json
-- Just (Array [Number 123456789 (-6)])
-- >>> readJSON "{\"asd\":\"sdf\", \"dfg\":\"fgh\"}" -- y_object.json
-- Just (Object [("asd",String "sdf"),("dfg",String "fgh")])
-- >>> readJSON "{\"asd\":\"sdf\"}" -- y_object_basic.json
-- Just (Object [("asd",String "sdf")])
-- >>> readJSON "{\"a\":\"b\",\"a\":\"c\"}" -- y_object_duplicated_key.json
-- Just (Object [("a",String "b"),("a",String "c")])
-- >>> readJSON "{\"a\":\"b\",\"a\":\"b\"}" -- y_object_duplicated_key_and_value.json
-- Just (Object [("a",String "b"),("a",String "b")])
-- >>> readJSON "{}" -- y_object_empty.json
-- Just (Object [])
-- >>> readJSON "{\"\":0}" -- y_object_empty_key.json
-- Just (Object [("",Number 0 0)])
-- >>> readJSON "{\"foo\\u0000bar\": 42}" -- y_object_escaped_null_in_key.json
-- Just (Object [("foo\NULbar",Number 42 0)])
-- >>> readJSON "{ \"min\": -1.0e+28, \"max\": 1.0e+28 }" -- y_object_extreme_numbers.json
-- Just (Object [("min",Number (-10) 27),("max",Number 10 27)])
-- >>> readJSON "{\"x\":[{\"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], \"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}" -- y_object_long_strings.json
-- Just (Object [("x",Array [Object [("id",String "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")]]),("id",String "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")])
-- >>> readJSON "{\"a\":[]}" -- y_object_simple.json
-- Just (Object [("a",Array [])])
-- >>> readJSON "{\"title\":\"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430\" }" -- y_object_string_unicode.json
-- Just (Object [("title",String "\1055\1086\1083\1090\1086\1088\1072 \1047\1077\1084\1083\1077\1082\1086\1087\1072")])
-- >>> readJSON "{\n\"a\": \"b\"\n}" -- y_object_with_newlines.json
-- Just (Object [("a",String "b")])
-- >>> readJSON "[\"\\u0060\\u012a\\u12AB\"]" -- y_string_1_2_3_bytes_UTF-8_sequences.json
-- Just (Array [String "`\298\4779"])
-- >>> readJSON "[\"\\uD801\\udc37\"]" -- y_string_accepted_surrogate_pair.json
-- Just (Array [String "\55297\56375"])
-- >>> readJSON "[\"\\ud83d\\ude39\\ud83d\\udc8d\"]" -- y_string_accepted_surrogate_pairs.json
-- Just (Array [String "\55357\56889\55357\56461"])
-- >>> readJSON "[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]" -- y_string_allowed_escapes.json
-- Just (Array [String "\"\\/\b\f\n\r\t"])
-- >>> readJSON "[\"\\\\u0000\"]" -- y_string_backslash_and_u_escaped_zero.json
-- Just (Array [String "\\u0000"])
-- >>> readJSON "[\"\\\"\"]" -- y_string_backslash_doublequotes.json
-- Just (Array [String "\""])
-- >>> readJSON "[\"a/*b*/c/*d//e\"]" -- y_string_comments.json
-- Just (Array [String "a/*b*/c/*d//e"])
-- >>> readJSON "[\"\\\\a\"]" -- y_string_double_escape_a.json
-- Just (Array [String "\\a"])
-- >>> readJSON "[\"\\\\n\"]" -- y_string_double_escape_n.json
-- Just (Array [String "\\n"])
-- >>> readJSON "[\"\\u0012\"]" -- y_string_escaped_control_character.json
-- Just (Array [String "\DC2"])
-- >>> readJSON "[\"\\uFFFF\"]" -- y_string_escaped_noncharacter.json
-- Just (Array [String "\65535"])
-- >>> readJSON "[\"asd\"]" -- y_string_in_array.json
-- Just (Array [String "asd"])
-- >>> readJSON "[ \"asd\"]" -- y_string_in_array_with_leading_space.json
-- Just (Array [String "asd"])
-- >>> readJSON "[\"\\uDBFF\\uDFFF\"]" -- y_string_last_surrogates_1_and_2.json
-- Just (Array [String "\56319\57343"])
-- >>> readJSON "[\"new\\u00A0line\"]" -- y_string_nbsp_uescaped.json
-- Just (Array [String "new\160line"])
-- >>> readJSON "[\"\1114111\"]" -- y_string_nonCharacterInUTF-8_U+10FFFF.json
-- Just (Array [String "\1114111"])
-- >>> readJSON "[\"\114687\"]" -- y_string_nonCharacterInUTF-8_U+1FFFF.json
-- Just (Array [String "\114687"])
-- >>> readJSON "[\"\65535\"]" -- y_string_nonCharacterInUTF-8_U+FFFF.json
-- Just (Array [String "\65535"])
-- >>> readJSON "[\"\\u0000\"]" -- y_string_null_escape.json
-- Just (Array [String "\NUL"])
-- >>> readJSON "[\"\\u002c\"]" -- y_string_one-byte-utf-8.json
-- Just (Array [String ","])
-- >>> readJSON "[\"\960\"]" -- y_string_pi.json
-- Just (Array [String "\960"])
-- >>> readJSON "[\"asd \"]" -- y_string_simple_ascii.json
-- Just (Array [String "asd "])
-- >>> readJSON "\" \"" -- y_string_space.json
-- Just (String " ")
-- >>> readJSON "[\"\\uD834\\uDd1e\"]" -- y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json
-- Just (Array [String "\55348\56606"])
-- >>> readJSON "[\"\\u0821\"]" -- y_string_three-byte-utf-8.json
-- Just (Array [String "\2081"])
-- >>> readJSON "[\"\\u0123\"]" -- y_string_two-byte-utf-8.json
-- Just (Array [String "\291"])
-- >>> readJSON "[\"\8232\"]" -- y_string_u+2028_line_sep.json
-- Just (Array [String "\8232"])
-- >>> readJSON "[\"\8233\"]" -- y_string_u+2029_par_sep.json
-- Just (Array [String "\8233"])
-- >>> readJSON "[\"\\u0061\\u30af\\u30EA\\u30b9\"]" -- y_string_uEscape.json
-- Just (Array [String "a\12463\12522\12473"])
-- >>> readJSON "[\"new\\u000Aline\"]" -- y_string_uescaped_newline.json
-- Just (Array [String "new\nline"])
-- >>> readJSON "[\"\DEL\"]" -- y_string_unescaped_char_delete.json
-- Just (Array [String "\DEL"])
-- >>> readJSON "[\"\\uA66D\"]" -- y_string_unicode.json
-- Just (Array [String "\42605"])
-- >>> readJSON "[\"\\u005C\"]" -- y_string_unicodeEscapedBackslash.json
-- Just (Array [String "\\"])
-- >>> readJSON "[\"\9026\12852\9026\"]" -- y_string_unicode_2.json
-- Just (Array [String "\9026\12852\9026"])
-- >>> readJSON "[\"\\uDBFF\\uDFFE\"]" -- y_string_unicode_U+10FFFE_nonchar.json
-- Just (Array [String "\56319\57342"])
-- >>> readJSON "[\"\\uD83F\\uDFFE\"]" -- y_string_unicode_U+1FFFE_nonchar.json
-- Just (Array [String "\55359\57342"])
-- >>> readJSON "[\"\\u200B\"]" -- y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json
-- Just (Array [String "\8203"])
-- >>> readJSON "[\"\\u2064\"]" -- y_string_unicode_U+2064_invisible_plus.json
-- Just (Array [String "\8292"])
-- >>> readJSON "[\"\\uFDD0\"]" -- y_string_unicode_U+FDD0_nonchar.json
-- Just (Array [String "\64976"])
-- >>> readJSON "[\"\\uFFFE\"]" -- y_string_unicode_U+FFFE_nonchar.json
-- Just (Array [String "\65534"])
-- >>> readJSON "[\"\\u0022\"]" -- y_string_unicode_escaped_double_quote.json
-- Just (Array [String "\""])
-- >>> readJSON "[\"\8364\119070\"]" -- y_string_utf8.json
-- Just (Array [String "\8364\119070"])
-- >>> readJSON "[\"a\DELa\"]" -- y_string_with_del_character.json
-- Just (Array [String "a\DELa"])
-- >>> readJSON "false" -- y_structure_lonely_false.json
-- Just (Boolean False)
-- >>> readJSON "42" -- y_structure_lonely_int.json
-- Just (Number 42 0)
-- >>> readJSON "-0.1" -- y_structure_lonely_negative_real.json
-- Just (Number (-1) (-1))
-- >>> readJSON "null" -- y_structure_lonely_null.json
-- Just Null
-- >>> readJSON "\"asd\"" -- y_structure_lonely_string.json
-- Just (String "asd")
-- >>> readJSON "true" -- y_structure_lonely_true.json
-- Just (Boolean True)
-- >>> readJSON "\"\"" -- y_structure_string_empty.json
-- Just (String "")
-- >>> readJSON "[\"a\"]\n" -- y_structure_trailing_newline.json
-- Just (Array [String "a"])
-- >>> readJSON "[true]" -- y_structure_true_in_array.json
-- Just (Array [Boolean True])
-- >>> readJSON " [] " -- y_structure_whitespace_array.json
-- Just (Array [])
--
-- >>> readJSON "[1.0]" -- number_1.0.json
-- Just (Array [Number 10 (-1)])
-- >>> readJSON "[1.000000000000000005]" -- number_1.000000000000000005.json
-- Just (Array [Number 1000000000000000005 (-18)])
-- >>> readJSON "[1000000000000000]\n" -- number_1000000000000000.json
-- Just (Array [Number 1000000000000000 0])
-- >>> readJSON "[10000000000000000999]" -- number_10000000000000000999.json
-- Just (Array [Number 10000000000000000999 0])
-- >>> readJSON "[1E-999]" -- number_1e-999.json
-- Just (Array [Number 1 (-999)])
-- >>> readJSON "[1E6]" -- number_1e6.json
-- Just (Array [Number 1 6])
-- >>> readJSON "{\"\233\":\"NFC\",\"e\769\":\"NFD\"}" -- object_key_nfc_nfd.json
-- Just (Object [("\233",String "NFC"),("e\769",String "NFD")])
-- >>> readJSON "{\"e\769\":\"NFD\",\"\233\":\"NFC\"}" -- object_key_nfd_nfc.json
-- Just (Object [("e\769",String "NFD"),("\233",String "NFC")])
-- >>> readJSON "{\"a\":1,\"a\":2}" -- object_same_key_different_values.json
-- Just (Object [("a",Number 1 0),("a",Number 2 0)])
-- >>> readJSON "{\"a\":1,\"a\":1}" -- object_same_key_same_value.json
-- Just (Object [("a",Number 1 0),("a",Number 1 0)])
-- >>> readJSON "{\"a\":0, \"a\":-0}\n" -- object_same_key_unclear_values.json
-- Just (Object [("a",Number 0 0),("a",Number 0 0)])
-- >>> readJSON "[\"\\uD800\"]" -- string_1_escaped_invalid_codepoint.json
-- Just (Array [String "\55296"])
-- >>> readJSON "[\"\\uD800\\uD800\"]" -- string_2_escaped_invalid_codepoints.json
-- Just (Array [String "\55296\55296"])
-- >>> readJSON "[\"\\uD800\\uD800\\uD800\"]" -- string_3_escaped_invalid_codepoints.json
-- Just (Array [String "\55296\55296\55296"])
-- >>> readJSON "[\"A\\u0000B\"]" -- string_with_escaped_NULL.json
-- Just (Array [String "A\NULB"])

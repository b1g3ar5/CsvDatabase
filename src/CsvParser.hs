
module CsvParser
    (
        Value, csvFile, line, pValue, pString, pNumber, pEmpty, parse, eshow
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>))
import Prelude hiding (id)
import Numeric (readSigned, readFloat)

-- We have 2 types, string or numbers = Maybe Double
-- So, strings are NOT NULL, NULL = Nothing in the Maybe Double
type Value a = Either String (Maybe a)

eshow :: (Show a, Eq a, Num a, RealFrac a) => Value a -> String
eshow (Left s) = s
eshow (Right (Just v)) = if v==fromInteger (round v) then show (round v) else show v
eshow (Right Nothing) = ""

csvFile :: Parser [[Value Double]]
csvFile = endBy line eol

line :: Parser [Value Double]
line = sepBy pValue (oneOf ",")

eol :: Parser String
eol = try (string "\r\n")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"

pValue :: Parser (Value Double)
pValue = value
    where value =   Right <$> pNa
                <|> Right <$> pNumber
                <|> Left  <$> pString
                <?> "csv value"

pNa :: CharParser () (Maybe Double)
pNa = try (string "n/a") >>  return Nothing
        <|> empty

pNumber :: CharParser () (Maybe Double)
pNumber = do s <- getInput
             case readSigned readFloat s of
               [(n, s')] -> Just n <$ setInput s'
               _ -> empty

pEmpty :: CharParser () (Maybe a)
pEmpty = do s <- getInput
            case s of
              "" -> Nothing <$ setInput s
              _ -> empty

pString :: CharParser () String
pString = many (noneOf ",\n\r")



module Parser (
    readOrThrow,
    readExpr,
    readExprList
 ) where

import LispVal
import Control.Monad.Except
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)



spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
				Left err  -> throwError $ Parser err
				Right val -> return val


readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

--                  --
-- Parse data types --
--                  --

escapedChars :: Parser String
escapedChars = do char '\\'
	  	  x <- oneOf "\\\"ntr"
                  case x of
                   '\\' -> do return [x]
                   '"' -> do return [x]
                   'n' -> do return "\n"
                   't' -> do return "\t"
                   'r' -> do return "\r"

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ many1 (noneOf "\"\\") <|> escapedChars
                 char '"'
                 return $ String (concat x)

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
	       rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
                 return num

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= return . Number . read

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

--                     --
-- Helpers for parsing --
--                     --
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint ""     = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False

parseChar :: Parser LispVal
parseChar = do try $ string "#\\"
               x <- parseCharName <|> anyChar
               return $ Character x

parseCharName = do x <- try (string "space" <|> string "newline")
                   case x of
                     "SPACE" -> do return ' '
                     "space" -> do return ' '
                     "newline" -> do return '\n'
                     "NEWLINE" -> do return '\n'

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseNumber
         <|> try parseBool
         <|> try parseChar
         <|> parseQuoted
         <|> do  char '('
                 x <- (try parseList) <|> parseDottedList
                 char ')'
                 return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                  head <- endBy parseExpr spaces
                  tail <- char '.' >> spaces >> parseExpr
                  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
              char '\''
              x <- parseExpr
              return $ List [Atom "quote", x]



{-
  Use Parser library function, parse, to return either parsed value or error.
  Parsec returns an Either, left for error and right for normal


readExpr :: String -> Strig
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
-}

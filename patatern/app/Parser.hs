{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text (Text)
import Data.Char
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import SyntaxTree

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment ";")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keyword :: Text -> Parser Text
keyword = L.symbol sc

isOperator :: Char -> Bool
isOperator c = c `elem` ['+', '-', '*', '/', '?']

symbol :: Parser Text
symbol = T.pack <$> lexeme chars
  where
    chars = (:) <$> symbolStart <*> symbolCont
    symbolStart = satisfy (\c -> isLower c || isOperator c)
    symbolCont = many $ satisfy (\c -> isOperator c || isAlphaNum c)

variable :: Parser Text
variable = T.pack <$> lexeme chars
  where
    chars = (:) <$> symbolStart <*> symbolCont
    symbolStart = upperChar
    symbolCont = many alphaNumChar

int :: Parser Integer
int = lexeme L.decimal

atom :: Parser (Term Text)
atom = choice
  [ between (keyword "(") (keyword ")") term
  , Symbol <$> symbol
  , Int <$> int
  , Var <$> variable
  ]

term :: Parser (Term Text)
term = do
    ts <- some atom
    return $ foldr1 (:<) ts

rule :: Parser (Rule Text)
rule = do
    lhs <- term
    keyword ":"
    rhs <- sepBy term (keyword ",")
    keyword "."
    return $ Rule lhs rhs

program :: Parser [Rule Text]
program = many rule <* eof

parseProgram :: String -> Text -> Either String [Rule Text]
parseProgram filename input =
  case parse (space *> program) filename input of
    Left err -> Left $ errorBundlePretty err
    Right res -> Right res

module LLang.Parser (system, Parser) where

import Data.Void (Void)
import LLang.Syntax (LHeader (..), LRule (LRule), LSystem (LSystem), symbols)
import Text.Megaparsec (MonadParsec (eof), Parsec, endBy1, oneOf, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, hspace, hspace1, punctuationChar, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

system :: Parser LSystem
system = do
  headers <- header `endBy1` space1
  rules <- rule `endBy1` space1
  eof
  return $ LSystem headers rules

header :: Parser LHeader
header = axiom <|> angle

axiom :: Parser LHeader
axiom = do
  string "axiom"
  hspace1
  Axiom <$> acceptedString

angle :: Parser LHeader
angle = do
  string "angle"
  hspace1
  Angle <$> decimal

rule :: Parser LRule
rule = do
  left <- acceptedChar
  hspace
  string "->"
  hspace
  LRule left <$> acceptedString

acceptedString :: Parser String
acceptedString = some acceptedChar

acceptedChar :: Parser Char
acceptedChar = oneOf (map fst symbols) <|> alphaNumChar <|> punctuationChar

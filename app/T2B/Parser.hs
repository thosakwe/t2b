module T2B.Parser where

import Control.Monad.Trans.Class (lift)
import Text.Parsec
import T2B

import qualified Data.ByteString.Char8 as BS

type Parser a = ParsecT String () (T2B) a

parser :: Parser ()
parser = do
  _ <- many $ choice [comment, command]
  return ()

comment :: Parser ()
comment = do
  _ <- char '#'
  _ <- manyTill anyChar endOfLine
  _ <- optionMaybe $ choice [crlf, newline]
  return ()

command :: Parser ()
command = choice $ map try [str, strl]

str :: Parser ()
str = do
  _ <- string "str"
  spaces
  contents <- expr
  lift . emit . BS.pack $ contents

strl :: Parser ()
strl = do
  spaces
  _ <- string "strl"
  spaces
  contents <- expr
  lift . emit . BS.pack $ contents ++ "\n"

expr :: Parser String
expr = choice [stringLiteral]

stringLiteral :: Parser String
stringLiteral = do
  let contents = many (noneOf "\"")
  between (char '"') (char '"') contents

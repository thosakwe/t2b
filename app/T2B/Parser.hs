module T2B.Parser where

import Data.ByteString (ByteString)
import Text.Parsec
    ( alphaNum,
      char,
      letter,
      oneOf,
      choice,
      (<|>),
      many,
      ParsecT, manyTill )
import Text.Parsec.Token (GenLanguageDef(LanguageDef), commentStart, commentEnd, commentLine, nestedComments, identStart, identLetter, opStart, opLetter, reservedOpNames, reservedNames, caseSensitive)
import T2B

import qualified Data.ByteString.Char8 as BS
import qualified Text.Parsec.Token as Tok
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (modify)
import Control.Monad.Trans (lift)

type Parser a = ParsecT String () (T2B) a

parser :: Parser ByteString
parser = do
  byteStrings <- many $ choice [command]
  return $ foldl (<>) BS.empty byteStrings

command :: Parser ByteString
command = do
  spaces
  choice $ [d, f, hex, len, strl, str]

d :: Parser ByteString
d = do
  _ <- Tok.reserved lexer "d"
  result <- expr
  -- TODO (thosakwe): Hex mode
  let value = (read $ BS.unpack result) :: Double
  return . BS.pack $ show value

f :: Parser ByteString
f = do
  _ <- Tok.reserved lexer "f"
  result <- expr
  -- TODO (thosakwe): Hex mode
  let value = (read $ BS.unpack result) :: Double
  return . BS.pack $ show value

hex :: Parser ByteString
hex = do
  _ <- Tok.reserved lexer "hex"
  lift . modify $ \state -> state { hexMode = True }
  return BS.empty

len :: Parser ByteString
len = do
  _ <- Tok.reserved lexer "len"
  result <- Tok.parens lexer command
  return . BS.pack . show $ (length $ BS.unpack result)

size :: Parser ByteString
size = do
  _ <- Tok.reserved lexer "size"
  result <- Tok.parens lexer command
  return . BS.pack . show $ BS.length result

str :: Parser ByteString
str = do
  _ <- Tok.reserved lexer "str"
  expr

strl :: Parser ByteString
strl = do
  _ <- Tok.reserved lexer "strl"
  contents <- expr
  return $ contents <> BS.pack "\n"

times :: Parser ByteString
times = do
  _ <- Tok.reserved lexer "times"
  countStr <- expr
  let count = (read $ BS.unpack countStr) :: Int
  -- This isn't gonna work, we're gonna need an actual AST now.
  x <- manyTill command $ Tok.reserved lexer "endtimes"
  _ <- Tok.reserved lexer "endtimes"
  return $ foldl (<>) BS.empty x

expr :: Parser ByteString
expr = do
  spaces
  choice [stringLiteral, float, integer, Tok.parens lexer expr]

-- Create a lexer for integers, floats
-- () is the state type for the TokenParser, which in this case has no state.
-- lexer :: Tok.TokenParser ()
lexer :: Tok.GenTokenParser String () T2B
lexer = Tok.makeTokenParser emptyDef

emptyDef :: GenLanguageDef String () T2B
emptyDef = LanguageDef
  {
    commentStart   = ""
    , commentEnd     = ""
    , commentLine    = "#"
    , nestedComments = False
    , identStart     = letter <|> char '_'
    , identLetter    = alphaNum <|> oneOf "_'"
    , opStart        = opLetter emptyDef
    , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames= []
    , reservedNames  = [
        "d", "f", "hex", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
        "len", "size", "str", "strl", "times", "endtimes"
      ]
    , caseSensitive  = True
  }

stringLiteral :: Parser ByteString
stringLiteral = BS.pack <$> Tok.stringLiteral lexer

float :: Parser ByteString
float = do
  value <- Tok.float lexer
  return . BS.pack $ show value

integer :: Parser ByteString
integer = do
  value <- Tok.integer lexer
  return . BS.pack $ show value

spaces :: Parser ()
spaces = do
  Tok.whiteSpace lexer
  return ()

-- float :: Parser Double
-- float = Tok.float lexer

-- integer :: Parser Double
-- integer = do
--   intValue <- Tok.integer lexer
--   return $ fromIntegral intValue

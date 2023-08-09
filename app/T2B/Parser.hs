module T2B.Parser where

import Control.Monad.Error.Class (throwError)
import T2B
import T2B.AST
import Text.Parsec (choice, many, manyTill, getPosition, parse, try, (<|>), optionMaybe)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (commentLine, reservedNames)

import qualified Text.Parsec.Token as Tok

-- | Parses a T2B file, or throws a syntax error if it's invalid.
-- Otherwise, returns a list of commands which can be executed via `runT2B`.
parseT2B :: String -> String -> T2B [AstNode Command]
parseT2B filePath input = do
  let parseResult = parse parser filePath input
  case parseResult of
    Left err -> throwError $ SyntaxError err
    Right ast -> return ast

parser :: Parser [AstNode Command]
parser = many command

command :: Parser (AstNode Command)
command = do
  spaces
  choice $ [d, endl, f, get, hex, len, macro, set, strl, str, times, macroExpansion]

d :: Parser (AstNode Command)
d = do
  pos <- getPosition
  _ <- Tok.reserved lexer "d"
  value <- expr
  return $ (pos, DCommand value)

endl :: Parser (AstNode Command)
endl = do
  pos <- getPosition
  _ <- Tok.reserved lexer "endl"
  return $ (pos, EndlCommand)

f :: Parser (AstNode Command)
f = do
  pos <- getPosition
  _ <- Tok.reserved lexer "f"
  value <- expr
  return $ (pos, FCommand value)

get :: Parser (AstNode Command)
get = do
  pos <- getPosition
  _ <- Tok.reserved lexer "get"
  value <- expr
  return $ (pos, GetCommand value)

hex :: Parser (AstNode Command)
hex = do
  pos <- getPosition
  _ <- Tok.reserved lexer "hex"
  return $ (pos, HexCommand)

len :: Parser (AstNode Command)
len = do
  pos <- getPosition
  _ <- Tok.reserved lexer "len"
  value <- expr
  return $ (pos, LenCommand value)

macro :: Parser (AstNode Command)
macro = do
  pos <- getPosition
  _ <- Tok.reserved lexer "macro"
  name <- expr
  params <- manyTill expr $ Tok.reserved lexer "begin"
  commands <- manyTill command $ Tok.reserved lexer "endmacro"
  return $ (pos, MacroCommand name params commands)

macroExpansion :: Parser (AstNode Command)
macroExpansion = do
  pos <- getPosition
  (_, name) <- identifier
  args <- many expr
  return (pos, MacroExpansion (pos, Reference name) args)

set :: Parser (AstNode Command)
set = do
  pos <- getPosition
  _ <- Tok.reserved lexer "set"
  key <- expr
  value <- expr
  return $ (pos, SetCommand key value)

size :: Parser (AstNode Command)
size = do
  pos <- getPosition
  _ <- Tok.reserved lexer "size"
  value <- expr
  return $ (pos, SizeCommand value)

str :: Parser (AstNode Command)
str = do
  pos <- getPosition
  _ <- Tok.reserved lexer "str"
  value <- expr
  return $ (pos, StrCommand value)

strl :: Parser (AstNode Command)
strl = do
  pos <- getPosition
  _ <- Tok.reserved lexer "strl"
  value <- expr
  return $ (pos, StrlCommand value)

times :: Parser (AstNode Command)
times = do
  pos <- getPosition
  _ <- Tok.reserved lexer "times"
  count <- expr
  commands <- manyTill command $ Tok.reserved lexer "endtimes"
  return $ (pos, TimesCommand count commands)

expr :: Parser (AstNode Expr)
expr = do
  spaces
  choice [stringLiteral, number, interpolation, reference]

-- Create a lexer for integers, floats
-- () is the state type for the TokenParser, which in this case has no state.
lexer :: Tok.TokenParser ()
-- lexer :: Tok.GenTokenParser Text () T2B
lexer = Tok.makeTokenParser emptyDef
  {
    commentLine = "#",
    reservedNames  = [
      "begin", "d", "endl", "endmacro", "f", "get", "hex", "i8", "i16", "i32", "i64", "u8", "u16",
      "u32", "u64", "len", "macro", "set", "size", "str", "strl", "times", "endtimes"
    ]
  }

stringLiteral :: Parser (AstNode Expr)
stringLiteral = do
  pos <- getPosition
  literal <- StringLiteral <$> Tok.stringLiteral lexer
  return (pos, literal)

number :: Parser (AstNode Expr)
number = try float <|> integer

float :: Parser (AstNode Expr)
float = do
  pos <- getPosition
  literal <- DoubleLiteral <$> Tok.float lexer
  return (pos, literal)

integer :: Parser (AstNode Expr)
integer = do
  pos <- getPosition
  value <- Tok.integer lexer
  let literal = IntLiteral $ fromIntegral value
  return (pos, literal)

interpolation :: Parser (AstNode Expr)
interpolation = do
  pos <- getPosition
  value <- Tok.parens lexer command
  return (pos, Interpolation value)

reference :: Parser (AstNode Expr)
reference = do
  pos <- getPosition
  (_, value) <- identifier
  return (pos, Reference value)

identifier :: Parser (AstNode String)
identifier = do
  spaces
  pos <- getPosition
  value <- Tok.identifier lexer
  return (pos, value)

spaces :: Parser ()
spaces = do
  Tok.whiteSpace lexer
  return ()

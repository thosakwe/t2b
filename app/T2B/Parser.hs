module T2B.Parser where

import Text.Parsec (choice, many, manyTill, getPosition)
import T2B.AST

import qualified Text.Parsec.Token as Tok
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

parser :: Parser [AstNode Command]
parser = many command

command :: Parser (AstNode Command)
command = do
  spaces
  choice $ [d, f, hex, len, strl, str]

d :: Parser (AstNode Command)
d = do
  pos <- getPosition
  _ <- Tok.reserved lexer "d"
  value <- expr
  return $ (pos, DCommand value)

f :: Parser (AstNode Command)
f = do
  pos <- getPosition
  _ <- Tok.reserved lexer "f"
  value <- expr
  return $ (pos, FCommand value)

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
  _ <- Tok.reserved lexer "endtimes"
  return $ (pos, TimesCommand count commands)

expr :: Parser (AstNode Expr)
expr = do
  spaces
  choice [stringLiteral, float, integer, Tok.parens lexer expr]

-- Create a lexer for integers, floats
-- () is the state type for the TokenParser, which in this case has no state.
lexer :: Tok.TokenParser ()
-- lexer :: Tok.GenTokenParser Text () T2B
lexer = Tok.makeTokenParser emptyDef

stringLiteral :: Parser (AstNode Expr)
stringLiteral = do
  pos <- getPosition
  literal <- StringLiteral <$> Tok.stringLiteral lexer
  return (pos, literal)

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

spaces :: Parser ()
spaces = do
  Tok.whiteSpace lexer
  return ()

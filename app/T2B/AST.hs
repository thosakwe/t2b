module T2B.AST where

import Text.Parsec (SourcePos)

type AstNode a = (SourcePos, a)

type Block = AstNode [AstNode Command]

data Command =
  DCommand (AstNode Expr)
  | EndlCommand
  | FCommand (AstNode Expr)
  | GetCommand (AstNode Expr)
  | SetCommand (AstNode Expr) (AstNode Expr)
  | HexCommand
  | LenCommand (AstNode Expr)
  | MacroCommand (AstNode Expr) [AstNode Expr] [AstNode Command]
  | MacroExpansion (AstNode Expr) [AstNode Expr]
  | SizeCommand (AstNode Expr)
  | StrCommand (AstNode Expr)
  | StrlCommand (AstNode Expr)
  | TimesCommand (AstNode Expr) [AstNode Command]
  deriving (Show)

data Expr =
  IntLiteral Int
  | DoubleLiteral Double
  | StringLiteral String
  | Interpolation (AstNode Command)
  | Reference String
  deriving (Show)

module T2B.Interpreter where

import Control.Monad.Trans (MonadTrans(lift), MonadIO (liftIO))
import Control.Monad.State (modify, gets, get, StateT (runStateT), withStateT, put)
import Data.ByteString (ByteString)
import T2B
import T2B.AST

import qualified Data.ByteString.Char8 as BS
import qualified T2B.Scope as Scope
import qualified T2B.Scope as Scope
import Control.Monad.Error.Class (throwError)
import Text.Parsec (SourcePos)

exec :: [AstNode Command] -> T2B ByteString
exec commands = do
  results <- mapM execCommand commands
  return $ foldl (<>) BS.empty results

execCommand :: AstNode Command -> T2B ByteString

execCommand (_, DCommand expr) = do
  result <- execExpr expr
  str <- readDouble result
  return . BS.pack . show $ str

execCommand (_, EndlCommand) = return $ BS.pack "\n"

execCommand (_, FCommand expr) = do
  result <- execExpr expr
  str <- readDouble result
  return . BS.pack . show $ str

execCommand (pos, GetCommand expr) = do
  varName <- BS.unpack <$> execExpr expr
  lookupVar pos varName

execCommand (_, HexCommand) = do
  lift . modify $ \state -> state { hexMode = True }
  return BS.empty

execCommand (_, LenCommand expr) = do
  result <- execExpr expr
  return . BS.pack . show $ length (BS.unpack result)

execCommand (_, MacroCommand macroNameExpr paramExprs commands) = do
  currentMacros <- lift $ gets macros
  -- Read the name and params for this macro
  macroName <- readVarName macroNameExpr
  params <- mapM readVarName paramExprs
  let macro = (macroName, params, commands)
  -- Insert the new macro into the scope
  let newMacros = Scope.insert macroName macro currentMacros
  lift . modify $ \state -> state { macros = newMacros }
  return BS.empty

execCommand (pos, MacroExpansion varNameExpr argsExprs) = do
  macroName <- readVarName varNameExpr
  macro <- lookupMacro pos macroName
  invokeMacro pos macro argsExprs

execCommand (_, SetCommand varNameExpr valueExpr) = do
  scope <- lift $ gets variables
  varName <- readVarName varNameExpr
  value <- execExpr valueExpr
  let newScope = Scope.insert varName value scope
  lift $ modify $ \state -> state { variables = newScope }
  return BS.empty

execCommand (_, SizeCommand expr) = do
  result <- execExpr expr
  return . BS.pack . show $ BS.length result

execCommand (_, StrCommand msgExpr) = do
  execExpr msgExpr

execCommand (_, StrlCommand msgExpr) = do
  msg <- execExpr msgExpr
  return $ msg <> BS.pack "\n"

execCommand (_, TimesCommand countExpr commands) = do
  countStr <- execExpr countExpr
  let count = read (BS.unpack countStr) :: Int
  let
    iter :: Int -> T2B ByteString
    iter = \i -> do
        -- Create a new child scope where the variable i is set
        currentState <- lift $ get
        let newScope = Scope.createChild $ variables currentState
        let scopeWithI = Scope.insert "i" (BS.pack $ show i) newScope
        -- Create a new child state with this scope
        let newState = currentState { variables = scopeWithI }
        -- Now, run the commands in the new state
        lift $ put newState
        -- In accordance with the original C++ T2B, don't go back to the old
        -- scope.
        -- Concat everything and return it
        exec commands
  results <- loop iter count
  return $ foldl (<>) BS.empty results

execExpr :: AstNode Expr -> T2B ByteString
execExpr (_, DoubleLiteral value) = return $ BS.pack $ show value
execExpr (_, IntLiteral value) = return $ BS.pack $ show value
execExpr (_, StringLiteral str) = return $ BS.pack str
execExpr (_, Interpolation commandNode) = execCommand commandNode

execExpr (pos, Reference macroName) = do
  macro <- lookupMacro pos macroName
  invokeMacro pos macro []

invokeMacro :: SourcePos -> Macro -> [AstNode Expr] -> T2B ByteString
invokeMacro _ (_, params, commands) argExprs = do
  currentScope <- lift $ gets variables
  args <- mapM execExpr argExprs
  -- Assign params to values from args
  let assignedParams = zip params args
  -- Insert all these values into the scope
  let newScope = Scope.insertAll assignedParams currentScope
  lift . modify $ \state -> state { variables = newScope }
  -- Run all the commands
  exec commands

readDouble :: ByteString -> T2B Double
readDouble bs =
  let value = read (BS.unpack bs) :: Double
  in return value

readVarName :: AstNode Expr -> T2B String
readVarName varNameExpr =
  case varNameExpr of
    (_, Reference name) -> return name
    _ -> BS.unpack <$> execExpr varNameExpr

loop :: (Int -> T2B a) -> Int -> T2B [a]
loop action count = do
  let
    looper :: (Int -> T2B a) -> Int -> [a] -> T2B (Int, [a])
    looper action' i acc =
      if i >= count then
        return (i, acc)
      else do
        result <- action' i
        looper action' (i + 1) (acc ++ [result])
  (_, results) <- looper action 0 []
  return results

lookupVar :: SourcePos -> String -> T2B ByteString
lookupVar pos varName = do
  scope <- lift $ gets variables
  case Scope.lookup varName scope of
    Nothing -> throwError $ MissingVar pos varName
    Just value -> return value

lookupMacro :: SourcePos -> String -> T2B Macro
lookupMacro pos macroName = do
  scope <- lift $ gets macros
  case Scope.lookup macroName scope of
    Nothing -> throwError $ MissingMacro pos macroName
    Just value -> return value

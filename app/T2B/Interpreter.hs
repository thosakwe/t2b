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
  scope <- lift $ gets variables
  varName <- BS.unpack <$> execExpr expr
  case Scope.lookup varName scope of
    Nothing -> throwError $ MissingVar pos varName
    Just value -> return value

execCommand (_, HexCommand) = do
  lift . modify $ \state -> state { hexMode = True }
  return BS.empty

execCommand (_, LenCommand expr) = do
  result <- execExpr expr
  return . BS.pack . show $ length (BS.unpack result)

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
        output <- mapM execCommand commands
        -- Go back to the old state
        lift $ put currentState
        -- Concat everything and return it
        return $ foldl (<>) BS.empty output
  results <- loop iter count
  return $ foldl (<>) BS.empty results

execExpr :: AstNode Expr -> T2B ByteString
execExpr (_, DoubleLiteral value) = return $ BS.pack $ show value
execExpr (_, IntLiteral value) = return $ BS.pack $ show value
execExpr (_, StringLiteral str) = return $ BS.pack str
execExpr (_, Interpolation commandNode) = execCommand commandNode

readDouble :: ByteString -> T2B Double
readDouble bs =
  let value = read (BS.unpack bs) :: Double
  in return value

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

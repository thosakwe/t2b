module T2B.Interpreter where

import Data.ByteString (ByteString)
import T2B
import T2B.AST

import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.State (modify)

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
  let run = do
        output <- mapM execCommand commands
        return $ foldl (<>) BS.empty output
  results <- loop count run  []
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

loop :: Int -> T2B a -> [a] -> T2B [a]
loop n action acc =
  if n <= 0
    then return acc
  else do
    result <- action
    let nextAcc = acc ++ [result]
    loop (n - 1) action nextAcc

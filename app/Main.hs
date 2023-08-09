module Main where

import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.IO.Class (liftIO)
import System.Environment
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import T2B (runT2B, T2BError (InvalidCommand, SyntaxError, MissingVar))
import T2B.Interpreter (exec)
import T2B.Parser (parseT2B)
import Text.Parsec (parse)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified T2B.Parser as T2B

main :: IO ()
main = do
  args <- getArgs

  let filePath = case args of
        [] -> "stdin"
        inputFile:_ -> inputFile

  input <- case args of
    [] -> getContents
    inputFile:_ -> readFile inputFile

  result <- runT2B $ do
    ast <- parseT2B filePath input
    exec ast

  case result of
    Right bs -> BS.putStr bs

    -- Handle errors
    Left (InvalidCommand command) -> do
      hPutStrLn stderr $ "Invalid command: " ++ (Text.unpack command)
      exitFailure
    Left (MissingVar pos varName) -> do
      hPutStrLn stderr $ (show pos) ++ ": Missing variable: " ++ varName
      exitFailure
    Left (SyntaxError parseError) -> do
      hPutStrLn stderr $ "Syntax error: " ++ show parseError

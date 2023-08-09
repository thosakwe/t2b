module Main where

import Control.Monad.Error.Class (MonadError(throwError))
import System.Environment
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import T2B (runT2B, T2BError (InvalidCommand, SyntaxError))
import Text.Parsec (runParserT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified T2B.Parser as T2B

main :: IO ()
main = do
  args <- getArgs

  input <- case args of
    [] -> getContents
    inputFile:_ -> readFile inputFile

  result <- runT2B $ do
    parseResult <- runParserT T2B.parser () "stdin" input
    case parseResult of
      Left err -> throwError $ SyntaxError err
      Right _ -> return ()

  case result of
    Right byteString -> BS.putStr byteString

    -- Handle errors
    Left (InvalidCommand command) -> do
      hPutStrLn stderr $ "Invalid command: " ++ (Text.unpack command)
      exitFailure
    Left (SyntaxError parseError) -> do
      hPutStrLn stderr $ "Syntax error: " ++ show parseError

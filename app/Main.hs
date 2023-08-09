module Main where

import T2B (runT2B, T2BError (InvalidCommand, SyntaxError))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified T2B.Parser as T2B
import Text.Parsec (runParser, runParserT)
import Control.Monad.Error.Class (MonadError(throwError))

main :: IO ()
main = do
  input <- getContents
  result <- runT2B $ do
    -- TODO (thosakwe): Parse input here
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

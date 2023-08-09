module Main where

import Control.Monad.Error.Class (MonadError(throwError))
import System.Environment
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import T2B (runT2B, T2BError (InvalidCommand, SyntaxError))
import Text.Parsec (parse)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified T2B.Parser as T2B
import Control.Monad.IO.Class (liftIO)

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
    let parseResult = parse T2B.parser filePath input
    -- parseResult <- runParserT T2B.parser () filePath input

    case parseResult of
      Left err -> throwError $ SyntaxError err
      Right ast -> do
        liftIO . putStrLn $ show ast
        return BS.empty
      -- Right bs -> return bs

  case result of
    Right byteString -> BS.putStr byteString

    -- Handle errors
    Left (InvalidCommand command) -> do
      hPutStrLn stderr $ "Invalid command: " ++ (Text.unpack command)
      exitFailure
    Left (SyntaxError parseError) -> do
      hPutStrLn stderr $ "Syntax error: " ++ show parseError

module Main where

import T2B (runT2B, T2BError (InvalidCommand))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

main :: IO ()
main = do
  result <- runT2B $ do
    -- TODO (thosakwe): Parse input here
    return ()

  case result of
    Right byteString -> BS.putStr byteString

    -- Handle errors
    Left (InvalidCommand command) -> do
      hPutStrLn stderr $ "Invalid command: " ++ (Text.unpack command)
      exitFailure

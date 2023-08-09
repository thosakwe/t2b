module T2B where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT (runStateT))
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Control.Monad.Writer (tell)
import Data.ByteString (ByteString)
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)
import T2B.Scope (Scope)
import Text.Parsec (ParseError)

import qualified T2B.Scope as Scope

-- | The T2B monad represents a computation in the T2B language.
-- It combines error handling, state management, output, and IO operations.
type T2B = ExceptT T2BError (WriterT ByteString (StateT T2BState IO))

-- | Represents different error cases that can occur during T2B parsing.
data T2BError
  = InvalidCommand Text -- ^ An error indicating an invalid command encountered during parsing.
  | SyntaxError ParseError
  deriving (Show)

-- | The state of the T2B parser, including the current scope of variables.
data T2BState = T2BState
  { variables :: Scope Text -- ^ The scope of variables available during parsing.
  } deriving (Show)

-- | Creates an initial empty state for the T2B parser.
emptyState :: T2BState
emptyState = T2BState
  { variables = Scope.empty
  }

-- | Writes a bytestring to the output.
emit :: ByteString -> T2B ()
emit b = lift $ tell b

-- | Logs a message to stderr.
logToStderr :: String -> T2B ()
logToStderr = liftIO . (hPutStrLn stderr)

-- | Runs a T2B action, and returns the resulting ByteString if no error
-- occurred.
runT2B :: T2B () -> IO (Either T2BError ByteString)
runT2B action = do
  result <- runStateT (runWriterT (runExceptT action)) emptyState
  case result of
    ((Left err, _), _) -> return $ Left err
    ((Right _, bs), _) -> return $ Right bs

module T2B
  ( T2B
  , T2BError(..)
  , T2BState(..)
  , emptyState
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Text (Text)
import T2B.Scope (Scope)

import qualified T2B.Scope as Scope
import Control.Monad.Trans.Except (ExceptT)

-- | The T2B monad represents a computation in the T2B language.
-- It combines error handling, state management, and IO operations.
type T2B a = ExceptT T2BError (StateT T2BState IO) a

-- | Represents different error cases that can occur during T2B parsing.
data T2BError
  = InvalidCommand Text -- ^ An error indicating an invalid command encountered during parsing.
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

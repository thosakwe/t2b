module T2B.Scope where

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as Map

-- | Represents a lexical scope containing variable bindings.
data Scope a
  = RootScope (Map String a)
  | ChildScope (Scope a, Map String a)
  deriving (Show)

-- | Creates an empty root scope with no variable bindings.
empty :: Scope a
empty = RootScope Map.empty

-- | Looks up a variable in a scope. It searches the current scope and, if not
-- found, delegates to its parent scope.
lookup :: String -> Scope a -> Maybe a
lookup key (RootScope m) = Map.lookup key m
lookup key (ChildScope (parent, m)) =
  case Map.lookup key m of
    Just value -> Just value
    Nothing -> T2B.Scope.lookup key parent

-- | Inserts a variable binding into a scope.
insert :: String -> a -> Scope a -> Scope a
insert key value (RootScope m) = RootScope $ Map.insert key value m
insert key value (ChildScope (parent, m)) = ChildScope (parent, Map.insert key value m)

-- | Creates a new child scope based on an existing scope.
-- The child scope inherits the parent's bindings but can have its own additional bindings.
createChild :: Scope a -> Scope a
createChild scope = ChildScope (scope, Map.empty)

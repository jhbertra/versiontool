module Types
  ( CommitType(..)
  , Commit(..)
  , Version(..)
  ) where

data CommitType
  = Build
  | Chore
  | Ci
  | Docs
  | Feat
  | Fix
  | Perf
  | Refactor
  | Style
  | Test
  deriving (Eq, Ord, Show)

data Commit =
  Commit
    { _commitHash    :: String
    , _commitType    :: CommitType
    , _commitScope   :: Maybe String
    , _commitSummary :: String
    , _commitBody    :: String
    , _commitRefs    :: [String]
    }
  deriving (Eq, Ord, Show)

data Version =
  Version
    { _versionMajor :: Int
    , _versionMinor :: Int
    , _versionPatch :: Int
    }
  deriving (Eq, Ord, Show)

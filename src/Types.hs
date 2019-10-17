{-# LANGUAGE RecordWildCards #-}

module Types
  ( CommitType(..)
  , Commit(..)
  , Version(..)
  )
where

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
    { _commitHash :: String
    , _commitType :: CommitType
    , _commitScope :: Maybe String
    , _commitSummary :: String
    , _commitBody :: String
    , _commitRefs :: [String]
    , _commitShortHash :: String
    }
  deriving (Eq, Ord, Show)

data Version =
  Version
    { _versionMajor :: Int
    , _versionMinor :: Int
    , _versionPatch :: Int
    }
  deriving (Eq, Ord)

instance Show Version where
  show Version {..} =
    show _versionMajor ++ "." ++ show _versionMinor ++ "." ++ show _versionPatch

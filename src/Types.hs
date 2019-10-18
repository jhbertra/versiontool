{-# LANGUAGE RecordWildCards #-}

module Types
  ( CommitType(..)
  , Commit(..)
  , Version(..)
  , PreReleaseVariant(..)
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
  | Revert
  | Style
  | Test
  deriving (Eq, Ord, Show)

data Commit =
  Commit
    { _commitType :: CommitType
    , _commitScope :: Maybe String
    , _commitSummary :: String
    , _commitBody :: String
    , _commitRefs :: [String]
    , _commitHash :: String
    , _commitShortHash :: String
    }
  deriving (Eq, Ord, Show)

data Version =
  Version
    { _versionMajor :: Int
    , _versionMinor :: Int
    , _versionPatch :: Int
    , _versionPreRelease :: Maybe PreReleaseVariant
    }
  deriving (Eq, Ord)

data PreReleaseVariant =
  PreReleaseVariant
    { _preReleaseIdentifier :: String
    , _preReleaseNumber     :: Int
    }
  deriving (Eq, Ord)

instance Show Version where
  show Version {..} =
    show _versionMajor
      ++ "."
      ++ show _versionMinor
      ++ "."
      ++ show _versionPatch
      ++ case _versionPreRelease of
           Just PreReleaseVariant {..} ->
             "-" ++ _preReleaseIdentifier ++ show _preReleaseNumber
           Nothing -> ""

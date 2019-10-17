{-# LANGUAGE RecordWildCards #-}

module SemVer
  ( getNextVersion
  )
where

import           Data.List
import           Types

getNextVersion :: Version -> [Commit] -> Maybe Version
getNextVersion Version {..} commits
  | any hasBreakingChange commits = Just $ Version (_versionMajor + 1) 0 0
  | any hasFeatureChange commits = Just
  $ Version _versionMajor (_versionMinor + 1) 0
  | any hasFixChange commits = Just
  $ Version _versionMajor _versionMinor (_versionPatch + 1)
  | otherwise = Nothing

hasBreakingChange :: Commit -> Bool
hasBreakingChange = isInfixOf "BREAKING CHANGE" . _commitBody

hasFeatureChange :: Commit -> Bool
hasFeatureChange = (== Feat) . _commitType

hasFixChange :: Commit -> Bool
hasFixChange = (== Fix) . _commitType


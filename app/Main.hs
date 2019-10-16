{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Data.List
import           Data.Maybe
import           Git
import           System.Console.CmdArgs
import           Types

data Versiontool
  = Analyze
  | Changelog
  deriving (Show, Data, Typeable)

analyze = Analyze

changelog = Changelog

main :: IO ()
main = handle =<< cmdArgs (modes [analyze, changelog])

handle :: Versiontool -> IO ()
handle Analyze =
  maybe
    (pure ())
    (\Version {..} ->
       putStrLn $
       show _versionMajor ++
       "." ++ show _versionMinor ++ "." ++ show _versionPatch) =<<
  handleAnalyze (Version 0 0 0) <$> getCommitsSinceLastRelease
handle Changelog = error "Not implemented"

handleAnalyze :: Version -> [Commit] -> Maybe Version
handleAnalyze Version {..} commits
  | any hasBreakingChange commits = Just $ Version (_versionMajor + 1) 0 0
  | any hasFeatureChange commits =
    Just $ Version _versionMajor (_versionMinor + 1) 0
  | any hasFixChange commits =
    Just $ Version _versionMajor _versionMinor (_versionPatch + 1)
  | otherwise = Nothing

hasBreakingChange :: Commit -> Bool
hasBreakingChange = isInfixOf "BREAKING CHANGE" . _commitBody

hasFeatureChange :: Commit -> Bool
hasFeatureChange = (== Feat) . _commitType

hasFixChange :: Commit -> Bool
hasFixChange = (== Fix) . _commitType

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Time
import           Git
import           System.Console.CmdArgs
import           Types

data Versiontool
  = Analyze
  | Changelog
      { title :: String
      , url   :: String
      }
  deriving (Show, Data, Typeable)

analyze = Analyze

changelog = Changelog { title = def, url = def }

main :: IO ()
main = handle =<< cmdArgs (modes [analyze, changelog])

handle :: Versiontool -> IO ()
handle Analyze =
  maybe
      (pure ())
      (\Version {..} ->
        putStrLn
          $  show _versionMajor
          ++ "."
          ++ show _versionMinor
          ++ "."
          ++ show _versionPatch
      )
    =<< handleAnalyze
    <$> getCurrentVersion
    <*> getCommitsSinceLastRelease
handle Changelog {..} =
  join
    $   handleChangelog title url
    <$> getCurrentVersion
    <*> getCommitsSinceLastRelease

handleAnalyze :: Version -> [Commit] -> Maybe Version
handleAnalyze Version {..} commits
  | any hasBreakingChange commits = Just $ Version (_versionMajor + 1) 0 0
  | any hasFeatureChange commits = Just
  $ Version _versionMajor (_versionMinor + 1) 0
  | any hasFixChange commits = Just
  $ Version _versionMajor _versionMinor (_versionPatch + 1)
  | otherwise = Nothing

handleChangelog :: String -> String -> Version -> [Commit] -> IO ()
handleChangelog title url Version {..} commits
  | null bugFixes && null features && null breakingChanges = pure ()
  | otherwise = do
    putStr "*"
    putStr
      $  show _versionMajor
      ++ "."
      ++ show _versionMinor
      ++ "."
      ++ show _versionPatch
    putStr " "
    putStr title
    putStr "*"
    putStr " ("
    putStr =<< show . localDay . zonedTimeToLocalTime <$> getZonedTime
    putStrLn ")"
    putStrLn ""
    unless (null breakingChanges) $ do
      putStrLn "*BREAKING CHANGES*"
      forM_ breakingChanges $ \Commit {..} -> do
        putStr "• "
        forM_ _commitScope
          $ \scope -> putStr "*" >> putStr scope >> putStr "*: "
        putStrLn . last $ splitOn "BREAKING CHANGE: " _commitBody
    unless (null features) $ do
      putStrLn "*New Features*"
      forM_ features $ \Commit {..} -> do
        putStr "• "
        forM_ _commitScope
          $ \scope -> putStr "*" >> putStr scope >> putStr "*: "
        putStrLn _commitSummary
    unless (null bugFixes) $ do
      putStrLn "*Bug Fixes*"
      forM_ bugFixes $ \Commit {..} -> do
        putStr "• "
        forM_ _commitScope
          $ \scope -> putStr "*" >> putStr scope >> putStr "*: "
        putStrLn _commitSummary
 where
  bugFixes        = filter ((== Fix) . _commitType) commits
  features        = filter ((== Feat) . _commitType) commits
  breakingChanges = filter (isInfixOf "BREAKING CHANGE" . _commitBody) commits

hasBreakingChange :: Commit -> Bool
hasBreakingChange = isInfixOf "BREAKING CHANGE" . _commitBody

hasFeatureChange :: Commit -> Bool
hasFeatureChange = (== Feat) . _commitType

hasFixChange :: Commit -> Bool
hasFixChange = (== Fix) . _commitType

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Time
import           Git
import           SemVer
import           System.Console.CmdArgs
import           Types

data Versiontool
  = Analyze
  | Changelog
      { title :: String
      , githubUrl :: String
      }
  deriving (Show, Data, Typeable)

analyze = Analyze

changelog = Changelog { title = def, githubUrl = def }

main :: IO ()
main = handle =<< cmdArgs (modes [analyze, changelog])

handle :: Versiontool -> IO ()
handle Analyze        = handleAnalyze
handle Changelog {..} = do
  currentVersion <- getCurrentVersion
  commits        <- getCommitsSinceLastRelease
  case getNextVersion currentVersion commits of
    Just v  -> handleChangelog title githubUrl v commits
    Nothing -> pure ()

handleAnalyze :: IO ()
handleAnalyze =
  maybe (pure ()) print
    =<< getNextVersion
    <$> getCurrentVersion
    <*> getCommitsSinceLastRelease

handleChangelog :: String -> String -> Version -> [Commit] -> IO ()
handleChangelog title githubUrl Version {..} commits
  | null bugFixes && null features && null breakingChanges = pure ()
  | otherwise = do
    putStr "*"
    putStr
      $  show _versionMajor
      ++ "."
      ++ show _versionMinor
      ++ "."
      ++ show _versionPatch
    unless (null title) $ do
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
        putStr . takeWhile (/= '\n') . last $ splitOn "BREAKING CHANGE: "
                                                      _commitBody
        unless (null githubUrl)
          .  putStrLn
          $  " (<"
          ++ githubUrl
          ++ "/commit/"
          ++ _commitHash
          ++ "|"
          ++ _commitShortHash
          ++ ">)"
        when (null githubUrl) $ putStrLn ""
      putStrLn ""
    unless (null features) $ do
      putStrLn "*New Features*"
      forM_ features $ \Commit {..} -> do
        putStr "• "
        forM_ _commitScope
          $ \scope -> putStr "*" >> putStr scope >> putStr "*: "
        putStr _commitSummary
        unless (null githubUrl)
          .  putStrLn
          $  " (<"
          ++ githubUrl
          ++ "/commit/"
          ++ _commitHash
          ++ "|"
          ++ _commitShortHash
          ++ ">)"
        when (null githubUrl) $ putStrLn ""
      putStrLn ""
    unless (null bugFixes) $ do
      putStrLn "*Bug Fixes*"
      forM_ bugFixes $ \Commit {..} -> do
        putStr "• "
        forM_ _commitScope
          $ \scope -> putStr "*" >> putStr scope >> putStr "*: "
        putStr _commitSummary
        unless (null githubUrl)
          .  putStrLn
          $  " (<"
          ++ githubUrl
          ++ "/commit/"
          ++ _commitHash
          ++ "|"
          ++ _commitShortHash
          ++ ">)"
        when (null githubUrl) $ putStrLn ""
      putStrLn ""
 where
  bugFixes = sort $ filter ((== Fix) . _commitType) commits
  features = sort $ filter ((== Feat) . _commitType) commits
  breakingChanges =
    sort $ filter (isInfixOf "BREAKING CHANGE" . _commitBody) commits

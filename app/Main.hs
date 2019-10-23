{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Time
import           Git
import           SemVer
import           System.Console.CmdArgs
import           Types

data Versiontool
  = Next
   { tagPrefix :: String
   , variant :: Maybe String
   }
  | Current
    { tagPrefix :: String
    }
  | Changelog
    { title :: String
    , githubUrl :: String
    , tagPrefix :: String
    , variant :: Maybe String
    }
  deriving (Show, Data, Typeable)

next :: Versiontool
next = Next { tagPrefix = def, variant = def }

current :: Versiontool
current = Current { tagPrefix = def }

changelog :: Versiontool
changelog =
  Changelog { tagPrefix = def, title = def, githubUrl = def, variant = def }

main :: IO ()
main = handle =<< cmdArgs (modes [next, changelog, current])

handle :: Versiontool -> IO ()
handle Current {..}   = handleCurrent tagPrefix
handle Next {..}      = handleAnalyze tagPrefix variant
handle Changelog {..} = do
  currentVersion <- getCurrentVersion tagPrefix
  commits        <- getCommitsSinceLastRelease tagPrefix
  case getNextVersion variant currentVersion commits of
    Just v  -> handleChangelog title githubUrl v commits
    Nothing -> pure ()

handleCurrent :: String -> IO ()
handleCurrent tagPrefix = print =<< getCurrentVersion tagPrefix

handleAnalyze :: String -> Maybe String -> IO ()
handleAnalyze tagPrefix variant =
  maybe (pure ()) print
    =<< getNextVersion variant
    <$> getCurrentVersion tagPrefix
    <*> getCommitsSinceLastRelease tagPrefix

handleChangelog :: String -> String -> Version -> [Commit] -> IO ()
handleChangelog title githubUrl version commits
  | null bugFixes
    && null features
    && null breakingChanges
    && null performanceImprovements
  = pure ()
  | otherwise
  = do
    putStr "*"
    putStr $ show version
    unless (null title) $ do
      putStr " "
      putStr title
    putStr "*"
    putStr " ("
    putStr =<< show . localDay . zonedTimeToLocalTime <$> getZonedTime
    putStrLn ")"
    putStrLn ""
    printSection "BREAKING CHANGES" breakingChanges
      $ takeWhile (/= '\n')
      . last
      . splitOn "BREAKING CHANGE: "
      . _commitBody
    printSection "New Feature" features _commitSummary
    printSection "Bug Fixes"   bugFixes _commitSummary
    printSection "Performance Improvements"
                 performanceImprovements
                 _commitSummary
 where
  performanceImprovements = sort $ filter ((== Perf) . _commitType) commits
  bugFixes                = sort $ filter ((== Fix) . _commitType) commits
  features                = sort $ filter ((== Feat) . _commitType) commits
  breakingChanges =
    sort $ filter (isInfixOf "BREAKING CHANGE" . _commitBody) commits
  printSection sectionTitle sectionCommits renderCommit =
    unless (null sectionCommits) $ do
      putStrLn $ "*" ++ sectionTitle ++ "*"
      forM_ sectionCommits $ \commit@Commit {..} -> do
        putStr "• "
        forM_ _commitScope
          $ \scope -> putStr "*" >> putStr scope >> putStr "*: "
        putStr $ renderCommit commit
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

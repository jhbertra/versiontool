{-# LANGUAGE RecordWildCards #-}

module Git
  ( getCommitsSinceLastRelease
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Either
import           Data.Functor
import           Data.Maybe
import           System.Exit
import           System.Process
import           Text.Parsec
import           Text.Regex.PCRE
import           Types

data HashAndRefs =
  HashAndRefs
    { _hashAndRefsHash :: String
    , _hashAndRefsRefs :: [String]
    }
  deriving (Show)

data SummaryLine =
  SummaryLine
    { _summaryLineType    :: CommitType
    , _summaryLineScope   :: Maybe String
    , _summaryLineSummary :: String
    }

getCommitsSinceLastRelease :: IO [Commit]
getCommitsSinceLastRelease = do
  results <-
    runMaybeT $ do
      (exitCode, stdout, _) <-
        liftIO $
        readProcessWithExitCode "git" ["log", "--pretty=format:%H|%D"] ""
      logs <-
        case exitCode of
          ExitSuccess -> pure $ lines stdout
          _           -> MaybeT $ pure Nothing
      pure .
        map _hashAndRefsHash .
        reverse .
        takeWhile
          (\HashAndRefs {..} ->
             null _hashAndRefsRefs ||
             all
               (\r -> not $ r =~ "tag: .*[0-9]+\\.[0-9]+\\.[0-9]+")
               _hashAndRefsRefs) .
        rights $
        map (runParser hashAndRefsP () "line") logs
  fmap catMaybes . mapM readCommit $ fromMaybe [] results
  where
    hashAndRefsP = HashAndRefs <$> hashP <*> (string "|" *> refs)
    hashP = many1 hexDigit

readCommit :: String -> IO (Maybe Commit)
readCommit hash =
  runMaybeT $ do
    hash <- readField "H" hash
    summaryLineRaw <- readField "s" hash
    body <- readField "b" hash
    refsRaw <- readField "D" hash
    SummaryLine {..} <-
      case runParser summaryLine () "line" summaryLineRaw of
        Left _  -> MaybeT $ pure Nothing
        Right s -> MaybeT . pure $ Just s
    refsParsed <-
      case runParser refs () "line" refsRaw of
        Left _  -> MaybeT $ pure Nothing
        Right s -> MaybeT . pure $ Just s
    pure $
      Commit
        hash
        _summaryLineType
        _summaryLineScope
        _summaryLineSummary
        body
        refsParsed

readField :: String -> String -> MaybeT IO String
readField field hash = do
  (exitCode, stdout, _) <-
    liftIO $
    readProcessWithExitCode
      "git"
      ["log", "-1", hash, "--pretty=format:%" ++ field]
      ""
  case exitCode of
    ExitSuccess -> pure stdout
    _           -> MaybeT $ pure Nothing

summaryLine :: Parsec String () SummaryLine
summaryLine = SummaryLine <$> commitType <*> commitScope <*> commitSummary

commitType :: Parsec String () CommitType
commitType =
  (string "build" $> Build) <|> (string "chore" $> Chore) <|>
  (string "ci" $> Ci) <|>
  (string "docs" $> Docs) <|>
  (string "feat" $> Feat) <|>
  (string "fix" $> Fix) <|>
  (string "perf" $> Perf) <|>
  (string "refactor" $> Refactor) <|>
  (string "style" $> Style) <|>
  (string "test" $> Test)

commitScope :: Parsec String () (Maybe String)
commitScope = optionMaybe $ char '(' *> (many1 $ noneOf ")") <* char ')'

commitSummary :: Parsec String () String
commitSummary = string ": " *> many1 anyChar

refs :: Parsec String () [String]
refs = many1 (noneOf ",") `sepBy` string ", "

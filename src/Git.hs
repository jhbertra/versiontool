{-# LANGUAGE RecordWildCards #-}

module Git
  ( getCommitsSinceLastRelease
  , getCurrentVersion
  )
where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Either
import           Data.Functor
import           Data.List
import           Data.Maybe
import           SemVer
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

getCurrentVersion :: String -> IO Version
getCurrentVersion tagPrefix = do
  let regex = "tag: " ++ tagPrefix ++ ".*" ++ "(" ++ semVerRegex ++ ")"
  result <- runMaybeT $ do
    (exitCode, stdout, _) <- liftIO
      $ readProcessWithExitCode "git" ["log", "--pretty=format:%D"] ""
    logs <- case exitCode of
      ExitSuccess -> pure $ lines stdout
      _           -> MaybeT $ pure Nothing
    let parsedLines = map (runParser refs () "line") logs
    MaybeT
      .   pure
      . (\tag -> let [[_, v, _]] = tag =~ regex :: [[String]] in parseVersion v
        )
      <=< (MaybeT . pure . find (const True))
      .   map (head . filter (=~ regex))
      .   filter (any (=~ regex))
      .   rights
      $   parsedLines
  pure $ fromMaybe (Version 0 0 0 Nothing) result

getCommitsSinceLastRelease :: String -> IO [Commit]
getCommitsSinceLastRelease tagPrefix = do
  let regex = "tag: " ++ tagPrefix ++ ".*" ++ "(" ++ semVerRegex ++ ")"
  results <- runMaybeT $ do
    (exitCode, stdout, _) <- liftIO
      $ readProcessWithExitCode "git" ["log", "--pretty=format:%H|%D"] ""
    logs <- case exitCode of
      ExitSuccess -> pure $ lines stdout
      _           -> MaybeT $ pure Nothing
    pure
      . map _hashAndRefsHash
      . reverse
      . takeWhile
          (\HashAndRefs {..} ->
            null _hashAndRefsRefs
              || all (\r -> not $ r =~ regex) _hashAndRefsRefs
          )
      . rights
      $ map (runParser hashAndRefsP () "line") logs
  fmap catMaybes . mapM readCommit $ fromMaybe [] results
 where
  hashAndRefsP = HashAndRefs <$> hashP <*> (string "|" *> refs)
  hashP        = many1 hexDigit

readCommit :: String -> IO (Maybe Commit)
readCommit hash = runMaybeT $ do
  hash             <- readField "H" hash
  shortHash        <- readField "h" hash
  summaryLineRaw   <- readField "s" hash
  body             <- readField "b" hash
  refsRaw          <- readField "D" hash
  SummaryLine {..} <- case runParser summaryLine () "line" summaryLineRaw of
    Left  e -> MaybeT $ pure Nothing
    Right s -> MaybeT . pure $ Just s
  refsParsed <- case runParser refs () "line" refsRaw of
    Left  e -> MaybeT $ pure Nothing
    Right s -> MaybeT . pure $ Just s
  pure $ Commit _summaryLineType
                _summaryLineScope
                _summaryLineSummary
                body
                refsParsed
                hash
                shortHash

readField :: String -> String -> MaybeT IO String
readField field hash = do
  (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode
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
  (string "build" $> Build)
    <|> try (string "chore" $> Chore)
    <|> (string "ci" $> Ci)
    <|> (string "docs" $> Docs)
    <|> try (string "feat" $> Feat)
    <|> (string "fix" $> Fix)
    <|> (string "perf" $> Perf)
    <|> try (string "revert" $> Revert)
    <|> (string "refactor" $> Refactor)
    <|> (string "style" $> Style)
    <|> (string "test" $> Test)

commitScope :: Parsec String () (Maybe String)
commitScope = optionMaybe $ char '(' *> many1 (noneOf ")") <* char ')'

commitSummary :: Parsec String () String
commitSummary = string ": " *> many1 anyChar

refs :: Parsec String () [String]
refs = many1 (noneOf ",") `sepBy` string ", "

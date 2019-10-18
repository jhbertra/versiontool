{-# LANGUAGE RecordWildCards #-}

module SemVer
  ( getNextVersion
  , parseVersion
  , semVerRegex
  )
where

import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Types

semVerRegex :: String
semVerRegex = "[0-9]+\\.[0-9]+\\.[0-9]+(-[0-9a-zA-Z]+)?"

parseVersion :: String -> Maybe Version
parseVersion = rightToMaybe . runParser version () ""
 where
  rightToMaybe (Left  _) = Nothing
  rightToMaybe (Right a) = Just a

version :: Parsec String () Version
version =
  Version
    <$> int
    <*> (char '.' *> int)
    <*> (char '.' *> int)
    <*> optionMaybe preRelease

int :: Parsec String () Int
int = read <$> many1 digit

preRelease :: Parsec String () PreReleaseVariant
preRelease = char '-' >> PreReleaseVariant <$> identifier <*> int

identifier :: Parsec String () String
identifier = many1 . oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z']

getNextVersion :: Maybe String -> Version -> [Commit] -> Maybe Version
getNextVersion preRelease version@Version {..} commits
  | variantsMatch && hasReleasableChange = Just incrementPreRelease
  | variantsDifferent && hasReleasableChange = Just resetPreRelease
  | isJust _versionPreRelease && isNothing preRelease = Just resetPreRelease
  | any hasBreakingChange commits        = Just incrementMajorVersion
  | any hasFeatureChange commits         = Just incrementMinorVersion
  | any hasFixChange commits             = Just incrementPatchVersion
  | otherwise                            = Nothing
 where
  incrementPreRelease = version
    { _versionPreRelease = fmap
                             (\PreReleaseVariant {..} ->
                               PreReleaseVariant _preReleaseIdentifier
                                 $ _preReleaseNumber
                                 + 1
                             )
                             _versionPreRelease
    }
  resetPreRelease       = version { _versionPreRelease = newPreReleaseVariant }
  incrementMajorVersion = Version (_versionMajor + 1) 0 0 newPreReleaseVariant
  incrementMinorVersion =
    Version _versionMajor (_versionMinor + 1) 0 newPreReleaseVariant
  incrementPatchVersion =
    Version _versionMajor _versionMinor (_versionPatch + 1) newPreReleaseVariant
  variantsMatch =
    ((==) <$> fmap _preReleaseIdentifier _versionPreRelease <*> preRelease)
      == Just True
  variantsDifferent =
    ((==) <$> fmap _preReleaseIdentifier _versionPreRelease <*> preRelease)
      == Just False
  hasReleasableChange =
    any hasBreakingChange commits
      || any hasFeatureChange commits
      || any hasFixChange     commits
  hasBreakingChange    = isInfixOf "BREAKING CHANGE" . _commitBody
  hasFeatureChange     = (== Feat) . _commitType
  hasFixChange         = (== Fix) . _commitType
  newPreReleaseVariant = PreReleaseVariant <$> preRelease <*> Just 1


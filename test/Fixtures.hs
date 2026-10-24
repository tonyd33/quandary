module Fixtures
  ( listed
  , other
  , spaceOf
  , question
  , option
  , fixture
  , withCopy
  , modelOf
  , cleanModelOf
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Storage.Files
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit

listed :: Text -> OptionId
listed = Listed . Id

other :: Text -> OptionId
other = Other . Id

spaceOf :: [Question] -> Space
spaceOf qs = Space {grades = [], questions = Map.fromList [(q.id, q) | q <- qs], notes = Map.empty}

question :: Text -> Pick -> [NonEmpty OptionId] -> [Option] -> Question
question q pick raisedBy options =
  Question
    { id = Id q
    , title = ""
    , description = ""
    , pick
    , raisedBy
    , options
    , forbid = []
    , links = []
    }

option :: Text -> Option
option o =
  Option
    { id = Id o
    , title = ""
    , description = ""
    , needs = []
    , needsOneOf = Nothing
    , excludes = []
    , links = []
    , assessment = Nothing
    }

fixture :: FilePath
fixture = "test/fixtures/notes-sync"

-- | Run on a fresh copy of the fixture.
withCopy :: (FilePath -> IO a) -> IO a
withCopy k = withSystemTempDirectory "quandary" $ \dir -> do
  copyTree fixture dir
  k dir

copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  createDirectoryIfMissing True to
  entries <- listDirectory from
  mapM_
    ( \e -> do
        let src = from </> e
        isDir <- doesDirectoryExist src
        if isDir then copyTree src (to </> e) else copyFile src (to </> e)
    )
    entries

-- | Read and validate the space, failing only when validation blocks it.
modelOf :: FilePath -> IO Model
modelOf dir = readSpace dir >>= validated

-- | Read and validate the space, failing on any problem reading it.
cleanModelOf :: FilePath -> IO Model
cleanModelOf dir = do
  loaded <- readSpace dir
  loaded.problems @?= []
  validated loaded

validated :: Loaded -> IO Model
validated loaded = maybe (assertFailure "blocked") pure (validate loaded.sources).model

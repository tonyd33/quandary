module Quandary.Shell.Env
  ( Env (..)
  , analysis
  , withFresh
  , reportStale
  )
where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Quandary.Core.Analyse (Analysis (..), Standing (..))
import Quandary.Core.Analyse qualified as Analyse
import Quandary.Core.Types
import Quandary.Core.Validate
import Quandary.Shell.Render.Text qualified as Render
import Quandary.Shell.Solver.Cadical qualified as Cadical
import Quandary.Shell.Storage.Files qualified as Files
import System.Exit (ExitCode (..))

data Env = Env
  { dir :: FilePath
  , point :: Text
  , model :: Model
  , override :: Files.Override
  -- ^ Whether the command writes over a live edit lock.
  }

analysis :: Env -> IO Analysis
analysis env = Analyse.analyse Cadical.cadical env.model env.point

-- | Run the continuation on the named point's standing, or report the point stale.
withFresh :: Env -> Text -> (Standing -> IO ExitCode) -> IO ExitCode
withFresh env name k =
  Analyse.analyse Cadical.cadical env.model name >>= \case
    StaleAt cores -> reportStale env name cores
    Fresh s -> k s

reportStale :: Env -> Text -> [Set Literal] -> IO ExitCode
reportStale env name cores = ExitFailure 1 <$ Text.putStr (Render.status env.model name (StaleAt cores) Map.empty Nothing)

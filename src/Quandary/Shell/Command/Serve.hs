module Quandary.Shell.Command.Serve (run) where

import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setHost, setPort)
import Quandary.Shell.Browse (Route (..), withLive)
import Quandary.Shell.Render.Html (link)
import Quandary.Shell.Serve (app)
import System.Exit (ExitCode (..))
import System.IO (hFlush, stdout)

run :: FilePath -> Text -> Int -> IO ExitCode
run dir point port = withLive dir $ \live -> do
  let settings = setHost (fromString host) (setPort port (setBeforeMainLoop announce defaultSettings))
  ExitSuccess <$ runSettings settings (app live point port)
  where
    host = "127.0.0.1"
    announce = do
      Text.putStrLn ("serving " <> Text.pack dir <> " at http://" <> Text.pack host <> ":" <> Text.pack (show port) <> link (StatusAt point))
      hFlush stdout

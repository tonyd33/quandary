module Quandary.Shell.Options
  ( Options (..)
  , Command (..)
  , Action (..)
  , Format (..)
  , parse
  )
where

import Data.Text (Text)
import Data.Version (showVersion)
import Options.Applicative
import Paths_quandary (version)
import Quandary.Core.Analyse (Confirmation (..))
import Quandary.Core.Rulings (current)

data Options = Options
  { dir :: FilePath
  , point :: Text
  , command :: Command
  }

data Command
  = Check
  | OnModel Action

data Action
  = Status
  | Show Text
  | Why Text
  | Whatif [Text]
  | Accept [Text]
  | Defeat [Text]
  | Exhaust [Text]
  | Settle [Text]
  | Reopen [Text]
  | Defer Text
  | Prefer Text Text
  | PointNew Text (Maybe Text) Bool
  | PointRm Text
  | Adopt Text Confirmation
  | Rulings
  | Links
  | Diff Text Text
  | Graph Format

data Format = Mermaid | Dot

parse :: IO Options
parse = execParser (info (options <**> helper <**> versionOption) (fullDesc <> progDesc "Explore a design space"))

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version) (long "version" <> help "Show the version")

options :: Parser Options
options =
  Options
    <$> strOption (short 'C' <> metavar "DIR" <> value "." <> help "The space's directory")
    <*> strOption (long "point" <> metavar "NAME" <> value current <> help "The point to work at")
    <*> hsubparser
      ( subcommand "check" "Report every problem in the space and its points" (pure Check)
          <> modelCommand "status" "The frontier, then closed, deferred and dormant questions, then the points" (pure Status)
          <> modelCommand "show" "A question's options, verdicts, rulings and raised questions" (Show <$> positional "QUESTION")
          <> modelCommand "why" "Every set of rulings that makes an option impossible or forced" (Why <$> positional "OPTION")
          <> modelCommand "whatif" "What would move if the rulings were applied, as accept|defeat|reopen ID... groups" (Whatif <$> some (positional "VERB|ID"))
          <> modelCommand "accept" "Accept each option, with an optional reason after them" (Accept <$> some (positional "OPTION... [--] [REASON]"))
          <> modelCommand "defeat" "Defeat each option, for the reason after them" (Defeat <$> some (positional "OPTION... [--] REASON"))
          <> modelCommand "exhaust" "Say each question's listed options are all there is, for the reason after them" (Exhaust <$> some (positional "QUESTION... [--] REASON"))
          <> modelCommand "settle" "Defeat every option of each question still possible, for the reason after them" (Settle <$> some (positional "QUESTION... [--] REASON"))
          <> modelCommand "reopen" "Drop each option's ruling, inherited ones included, or each question's deferral" (Reopen <$> some (positional "ID"))
          <> modelCommand "defer" "Leave a question off the frontier" (Defer <$> positional "QUESTION")
          <> modelCommand "prefer" "Rank an option 0 to 10, or - to clear" (Prefer <$> positional "OPTION" <*> positional "N")
          <> modelCommand "point" "Create or remove a point" (hsubparser (pointNew <> pointRm))
          <> modelCommand "adopt" "Move a branch's rulings into the point it extends and remove it" (Adopt <$> positional "BRANCH" <*> flag Unconfirmed Confirmed (long "confirmed" <> help "The user confirmed a proposed branch's rulings"))
          <> modelCommand "rulings" "The point's rulings, inherited ones included" (pure Rulings)
          <> modelCommand "links" "Each link label in use, with how many links use it" (pure Links)
          <> modelCommand "diff" "How two points differ" (Diff <$> positional "POINT" <*> positional "POINT")
          <> modelCommand "graph" "The space as a Mermaid flowchart or a Graphviz digraph" (Graph <$> format)
      )

format :: Parser Format
format =
  option
    (eitherReader formatNamed)
    (long "format" <> metavar "mermaid|dot" <> value Mermaid <> help "The output format, mermaid by default")

formatNamed :: String -> Either String Format
formatNamed "mermaid" = Right Mermaid
formatNamed "dot" = Right Dot
formatNamed f = Left ("unknown format " <> f <> ", expected mermaid or dot")

pointNew :: Mod CommandFields Action
pointNew =
  subcommand
    "new"
    "Create an empty point, extending another if given"
    (PointNew <$> positional "NAME" <*> optional (positional "EXTENDS") <*> switch (long "proposed" <> help "Mark the point as holding an agent's proposals"))

pointRm :: Mod CommandFields Action
pointRm = subcommand "rm" "Remove a point no other point extends" (PointRm <$> positional "NAME")

subcommand :: String -> String -> Parser a -> Mod CommandFields a
subcommand name desc p = command name (info p (progDesc desc))

modelCommand :: String -> String -> Parser Action -> Mod CommandFields Command
modelCommand name desc p = subcommand name desc (OnModel <$> p)

positional :: String -> Parser Text
positional meta = strArgument (metavar meta)

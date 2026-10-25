-- | What a space's documents say, decoded but not yet checked.
module Quandary.Core.Source
  ( Document (..)
  , Loc (..)
  , Located (..)
  , Sources (..)
  , QuestionSource (..)
  , OptionSource (..)
  , AssessmentSource (..)
  , ConfigSource (..)
  , ForbidSource (..)
  , LinkSource (..)
  , NoteSource (..)
  , PointSource (..)
  , RulingSource (..)
  , GradeSource (..)
  , Body (..)
  , Section (..)
  , Problem (..)
  , Issue (..)
  , Field (..)
  )
where

import Data.Text (Text)
import Quandary.Core.Types

data Document
  = QuestionDoc Text
  | NoteDoc Text
  | PointDoc Text
  | -- | The space's `quandary.yaml`.
    ConfigDoc
  deriving (Eq, Ord, Show)

data Loc = Loc
  { document :: Document
  , line :: Maybe Int
  -- ^ Counted from 1 in the document.
  }
  deriving (Eq, Ord, Show)

data Located a = Located
  { at :: Loc
  , value :: a
  }
  deriving (Eq, Show, Functor)

data Sources = Sources
  { config :: ConfigSource
  , questions :: [QuestionSource]
  , notes :: [NoteSource]
  , points :: [PointSource]
  }
  deriving (Eq, Show)

data ConfigSource = ConfigSource
  { grades :: Maybe (Located [GradeSource])
  }
  deriving (Eq, Show)

data GradeSource = GradeSource
  { at :: Loc
  , name :: Maybe (Located Text)
  , desc :: Maybe (Located Text)
  , color :: Maybe (Located Text)
  }
  deriving (Eq, Show)

data QuestionSource = QuestionSource
  { id :: Located Text
  -- ^ The document's path id, as `keys/where`, or its directory when the
  -- file is named after it, as `keys/where/where`.
  , scope :: Text
  -- ^ What its option ids start with: the directory of its file, with a
  -- trailing `/`, or empty at the root.
  , title :: Maybe (Located Text)
  , pick :: Maybe (Located Text)
  , raisedBy :: Maybe (Located [Located [Located Text]])
  -- ^ Each entry an option, or a list of options taken together.
  , options :: [OptionSource]
  , forbid :: [ForbidSource]
  , links :: [LinkSource]
  , body :: Body
  }
  deriving (Eq, Show)

data OptionSource = OptionSource
  { key :: Located Text
  , title :: Maybe (Located Text)
  , needs :: Maybe (Located [Located Text])
  , needsOneOf :: Maybe (Located [Located Text])
  , excludes :: Maybe (Located [Located Text])
  , links :: [LinkSource]
  , assessment :: Maybe AssessmentSource
  }
  deriving (Eq, Show)

data AssessmentSource = AssessmentSource
  { at :: Loc
  , grade :: Maybe (Located Text)
  , unverified :: Maybe (Located Text)
  , why :: Maybe (Located Text)
  }
  deriving (Eq, Show)

data ForbidSource = ForbidSource
  { at :: Loc
  , literals :: Maybe (Located [Located Text])
  , why :: Maybe (Located Text)
  }
  deriving (Eq, Show)

data LinkSource = LinkSource
  { at :: Loc
  , rel :: Maybe (Located Text)
  , to :: Maybe (Located Text)
  , extra :: [(Text, Text)]
  }
  deriving (Eq, Show)

data NoteSource = NoteSource
  { id :: Located Text
  , title :: Maybe (Located Text)
  , links :: [LinkSource]
  , body :: Body
  }
  deriving (Eq, Show)

data PointSource = PointSource
  { name :: Located Text
  , extends :: Maybe (Located Text)
  , proposed :: Maybe (Located Text)
  , accept :: [RulingSource]
  , defeat :: [RulingSource]
  , drop :: Maybe (Located [Located Text])
  , defer :: Maybe (Located [Located Text])
  , prefer :: [(Located Text, Located Text)]
  , body :: Text
  }
  deriving (Eq, Show)

data RulingSource = RulingSource
  { option :: Located Text
  , title :: Maybe (Located Text)
  , reason :: Maybe (Located Text)
  }
  deriving (Eq, Show)

-- | A Markdown body split at its level-one and level-two headings.
data Body = Body
  { preamble :: Text
  , sections :: [Section]
  }
  deriving (Eq, Show)

data Section = Section
  { heading :: Located Text
  , level :: Int
  , text :: Text
  }
  deriving (Eq, Show)

data Problem = Problem
  { at :: Loc
  , issue :: Issue
  }
  deriving (Eq, Show)

data Issue
  = BadId Text
  | -- | An option key spelled `other`.
    ReservedKey
  | -- | Shares an id, ignoring case, with the node at the location.
    Collision Text Loc
  | Missing Field
  | EmptyList Field
  | BadPick Text
  | UnknownOption Text
  | UnknownQuestion Text
  | UnknownTarget Text
  | UnknownPoint Text
  | -- | Holds the same literals as the entry at the location.
    DuplicateForbid Loc
  | BadLabel Text
  | MissingDescription
  | MissingSection Text
  | DuplicateSection Text
  | ExtendsCycle [Text]
  | AcceptedAndDefeated OptionId
  | -- | A drop of an option no extended point rules.
    DropsNothing OptionId
  | -- | A drop of an option the point itself rules.
    DropsOwn OptionId
  | -- | Names a question where an option belongs.
    QuestionNotOption Text
  | -- | The title recorded and the option's title now.
    TitleMismatch OptionId Text Text
  | TitleOnOther OptionId
  | BadRank Text
  | EmptyReason
  | EmptyDesc
  | BadProposed Text
  | BadUnverified Text
  | -- | A grade name that is not letters, digits, `-` and `_`.
    BadGrade Text
  | BadColor Text
  | DuplicateGrade Text
  | -- | A grade the space does not declare.
    UnknownGrade Text
  | -- | An assessment in a space declaring no grades.
    NoGrades
  deriving (Eq, Show)

data Field
  = TitleField
  | PickField
  | RaisedByField
  | NeedsOneOfField
  | AllField
  | RelField
  | ToField
  | ReasonField
  | GradeField
  | GradesField
  | NameField
  deriving (Eq, Show)

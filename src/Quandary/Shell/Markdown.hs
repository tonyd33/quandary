module Quandary.Shell.Markdown
  ( Block (..)
  , Inline (..)
  , Alignment (..)
  , parse
  , parsePlain
  , plainText
  )
where

import Commonmark
import Commonmark.Extensions (HasPipeTable (..), HasStrikethrough (..), autolinkSpec, pipeTableSpec, strikethroughSpec)
import Commonmark.Extensions qualified as Extensions
import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)
import Data.Text qualified as Text

data Block
  = Paragraph [Inline]
  | -- | The heading's level, first and last line.
    Heading Int (Int, Int) [Inline]
  | CodeBlock Text
  | Quote [Block]
  | -- | The first number of an ordered list, Nothing for a bulleted one,
    -- and its items.
    List (Maybe Int) [[Block]]
  | Rule
  | RawBlock Text
  | -- | Each column's alignment, the header row, and the rows below it.
    Table [Alignment] [[Inline]] [[[Inline]]]
  deriving (Eq, Show)

data Inline
  = Str Text
  | SoftBreak
  | LineBreak
  | Code Text
  | RawInline Text
  | Emph [Inline]
  | Strong [Inline]
  | Strike [Inline]
  | Link Text [Inline]
  | Image Text [Inline]
  deriving (Eq, Show)

data Alignment = AlignDefault | AlignLeft | AlignCenter | AlignRight
  deriving (Eq, Show)

-- | Returns the Markdown's blocks, parsed with GitHub's tables,
-- strikethrough and autolinks.
parse :: Text -> [Block]
parse = parseWith (pipeTableSpec <> strikethroughSpec <> autolinkSpec <> defaultSyntaxSpec)

-- | Returns the CommonMark's blocks, no extensions.
parsePlain :: Text -> [Block]
parsePlain = parseWith defaultSyntaxSpec

parseWith :: SyntaxSpec Identity Inlines Blocks -> Text -> [Block]
parseWith spec t = case runIdentity (commonmarkWith spec "" t) of
  Right (Blocks bs) -> bs
  Left _ -> [Paragraph [Str t]]

newtype Inlines = Inlines [Inline]
  deriving (Show, Semigroup, Monoid)

newtype Blocks = Blocks [Block]
  deriving (Show, Semigroup, Monoid)

inlines :: Inlines -> [Inline]
inlines (Inlines is) = is

blocks :: Blocks -> [Block]
blocks (Blocks bs) = bs

one :: Inline -> Inlines
one i = Inlines [i]

block :: Block -> Blocks
block b = Blocks [b]

instance Rangeable Inlines where
  ranged _ = id

instance HasAttributes Inlines where
  addAttributes _ = id

instance IsInline Inlines where
  lineBreak = one LineBreak
  softBreak = one SoftBreak
  str t = one (Str t)
  entity t = one (Str t)
  escapedChar c = one (Str (Text.singleton c))
  emph is = one (Emph (inlines is))
  strong is = one (Strong (inlines is))
  link url _ is = one (Link url (inlines is))
  image url _ is = one (Image url (inlines is))
  code t = one (Code t)
  rawInline _ t = one (RawInline t)

instance HasStrikethrough Inlines where
  strikethrough is = one (Strike (inlines is))

-- | Give a heading the lines its range spans. A range ending at the first
-- column ends on the line before.
instance Rangeable Blocks where
  ranged (SourceRange ((from, to) : _)) (Blocks [Heading n _ is]) = Blocks [Heading n (sourceLine from, lastLine to) is]
    where
      lastLine p
        | sourceColumn p == 1 = sourceLine p - 1
        | otherwise = sourceLine p
  ranged _ bs = bs

instance HasAttributes Blocks where
  addAttributes _ = id

instance IsBlock Inlines Blocks where
  paragraph is = block (Paragraph (inlines is))
  plain is = block (Paragraph (inlines is))
  thematicBreak = block Rule
  blockQuote bs = block (Quote (blocks bs))
  codeBlock _ t = block (CodeBlock t)
  heading n is = block (Heading n (0, 0) (inlines is))
  rawBlock _ t = block (RawBlock t)
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) _ items = block (List Nothing (map blocks items))
  list (OrderedList start _ _) _ items = block (List (Just start) (map blocks items))

instance HasPipeTable Inlines Blocks where
  pipeTable aligns header rows = block (Table (map alignment aligns) (map inlines header) (map (map inlines) rows))

alignment :: Extensions.ColAlignment -> Alignment
alignment Extensions.LeftAlignedCol = AlignLeft
alignment Extensions.CenterAlignedCol = AlignCenter
alignment Extensions.RightAlignedCol = AlignRight
alignment Extensions.DefaultAlignedCol = AlignDefault

-- | Returns the inlines' text without their markup.
plainText :: [Inline] -> Text
plainText = foldMap inlineText

inlineText :: Inline -> Text
inlineText (Str t) = t
inlineText (Code t) = t
inlineText SoftBreak = " "
inlineText LineBreak = ""
inlineText (RawInline _) = ""
inlineText (Emph is) = plainText is
inlineText (Strong is) = plainText is
inlineText (Strike is) = plainText is
inlineText (Link _ is) = plainText is
inlineText (Image _ is) = plainText is

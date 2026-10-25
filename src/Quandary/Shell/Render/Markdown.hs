module Quandary.Shell.Render.Markdown
  ( markdown
  , codeAttr
  , headingAttr
  )
where

import Brick
import Brick.Widgets.Border (hBorder)
import Data.List (intersperse, transpose)
import Data.Text (Text)
import Data.Text qualified as Text
import Quandary.Shell.Markdown qualified as M

codeAttr :: AttrName
codeAttr = attrName "code"

headingAttr :: AttrName
headingAttr = attrName "heading"

-- | Returns the GitHub-flavoured Markdown's blocks one under another, code
-- blocks verbatim, tables in aligned columns.
markdown :: Text -> Widget n
markdown = blocks . M.parse

blocks :: [M.Block] -> Widget n
blocks [] = emptyWidget
blocks bs = vBox (intersperse (txt " ") (map block bs))

block :: M.Block -> Widget n
block (M.Paragraph is) = txtWrap (inlines is)
block (M.Heading n _ is) = withAttr headingAttr (txtWrap (Text.replicate n "#" <> " " <> inlines is))
block (M.CodeBlock code) = withAttr codeAttr (padLeft (Pad 2) (vBox (map (txt . verbatim) (Text.lines code))))
block (M.Quote bs) = txt "> " <+> blocks bs
block (M.List start items) = vBox (zipWith listItem (bullets start) items)
block M.Rule = hBorder
block (M.RawBlock html) = txtWrap html
block (M.Table aligns header rows) = table aligns header rows

-- | Returns the table's rows in columns two apart, each cell aligned as
-- its column says, a rule under the header row.
table :: [M.Alignment] -> [[M.Inline]] -> [[[M.Inline]]] -> Widget n
table aligns header rows = vBox (map txt (line headerCells : rule : map line bodyCells))
  where
    headerCells = map inlines header
    bodyCells = map (map inlines) rows
    widths = map (maximum . map Text.length) (transpose (headerCells : bodyCells))
    line r = Text.stripEnd (Text.intercalate "  " (zipWith3 aligned (aligns ++ repeat M.AlignDefault) widths r))
    rule = Text.intercalate "  " [Text.replicate w "\x2500" | w <- widths]
    aligned M.AlignRight w = Text.justifyRight w ' '
    aligned M.AlignCenter w = Text.center w ' '
    aligned _ w = Text.justifyLeft w ' '

listItem :: Text -> [M.Block] -> Widget n
listItem bullet item = txt bullet <+> blocks item

bullets :: Maybe Int -> [Text]
bullets Nothing = repeat "- "
bullets (Just start) = [Text.pack (show i) <> ". " | i <- [start ..]]

-- | Returns the line with tabs as spaces and an empty line as one space.
verbatim :: Text -> Text
verbatim l = case Text.replace "\t" "    " l of
  "" -> " "
  l' -> l'

inlines :: [M.Inline] -> Text
inlines = foldMap inline

inline :: M.Inline -> Text
inline (M.Str t) = t
inline M.SoftBreak = " "
inline M.LineBreak = "\n"
inline (M.Code t) = "`" <> t <> "`"
inline (M.RawInline t) = t
inline (M.Emph is) = inlines is
inline (M.Strong is) = inlines is
inline (M.Strike is) = "~~" <> inlines is <> "~~"
inline (M.Link url is) = inlines is <> " <" <> url <> ">"
inline (M.Image url is) = "[" <> inlines is <> "](" <> url <> ")"

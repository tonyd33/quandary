module Quandary.Shell.Render.Markdown
  ( markdown
  , codeAttr
  , headingAttr
  )
where

import Brick
import Brick.Widgets.Border (hBorder)
import CMark qualified as M
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text

codeAttr :: AttrName
codeAttr = attrName "code"

headingAttr :: AttrName
headingAttr = attrName "heading"

-- | Returns the Markdown's blocks one under another, code blocks verbatim.
markdown :: Text -> Widget n
markdown t = blocks ns
  where
    M.Node _ _ ns = M.commonmarkToNode [] t

blocks :: [M.Node] -> Widget n
blocks [] = emptyWidget
blocks ns = vBox (intersperse (txt " ") (map block ns))

block :: M.Node -> Widget n
block (M.Node _ ty ns) = case ty of
  M.HEADING n -> withAttr headingAttr (txtWrap (Text.replicate n "#" <> " " <> inlines ns))
  M.CODE_BLOCK _ code -> withAttr codeAttr (padLeft (Pad 2) (vBox (map (txt . verbatim) (Text.lines code))))
  M.BLOCK_QUOTE -> txt "> " <+> blocks ns
  M.LIST attrs -> vBox (zipWith listItem (bullets attrs) ns)
  M.THEMATIC_BREAK -> hBorder
  M.HTML_BLOCK html -> txtWrap html
  _ -> txtWrap (inlines ns)

listItem :: Text -> M.Node -> Widget n
listItem bullet (M.Node _ _ children) = txt bullet <+> blocks children

bullets :: M.ListAttributes -> [Text]
bullets attrs = case attrs.listType of
  M.BULLET_LIST -> repeat "- "
  M.ORDERED_LIST -> [Text.pack (show i) <> ". " | i <- [attrs.listStart ..]]

-- | Returns the line with tabs as spaces and an empty line as one space.
verbatim :: Text -> Text
verbatim l = case Text.replace "\t" "    " l of
  "" -> " "
  l' -> l'

inlines :: [M.Node] -> Text
inlines = foldMap inline

inline :: M.Node -> Text
inline (M.Node _ ty ns) = case ty of
  M.TEXT t -> t
  M.SOFTBREAK -> " "
  M.LINEBREAK -> "\n"
  M.CODE t -> "`" <> t <> "`"
  M.HTML_INLINE t -> t
  M.LINK url _ -> inlines ns <> " <" <> url <> ">"
  M.IMAGE url _ -> "[" <> inlines ns <> "](" <> url <> ")"
  _ -> inlines ns

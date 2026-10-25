{-# LANGUAGE TemplateHaskell #-}

module Quandary.Shell.Assets
  ( asset
  , assetPath
  )
where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Numeric (showHex)

-- | Returns the media type and content of the file served at
-- `/assets/NAME`.
asset :: Text -> Maybe (ByteString, ByteString)
asset "style.css" = Just (css, $(embedFile "web/style.css"))
asset "graph.js" = Just (js, $(embedFile "web/graph.js"))
asset "box.js" = Just (js, $(embedFile "web/box.js"))
asset "graph-style.js" = Just (js, $(embedFile "web/graph-style.js"))
asset "explain.js" = Just (js, $(embedFile "web/explain.js"))
asset "cytoscape.min.js" = Just (js, $(embedFile "web/vendor/cytoscape/cytoscape.min.js"))
asset "cytoscape-dagre.min.js" = Just (js, $(embedFile "web/vendor/cytoscape-dagre/cytoscape-dagre.min.js"))
asset "cytoscape-expand-collapse.js" =
  Just (js, $(embedFile "web/vendor/cytoscape-expand-collapse/cytoscape-expand-collapse.js"))
asset _ = Nothing

-- | Returns the path the named asset is served at, versioned by the
-- content of every asset so a browser never keeps one from another build.
assetPath :: Text -> Text
assetPath name = "/assets/" <> name <> "?v=" <> version

version :: Text
version = Text.pack (showHex (ByteString.foldl' step 0xcbf29ce484222325 everything) "")
  where
    everything = foldMap (maybe "" snd . asset) names
    names =
      [ "style.css"
      , "graph.js"
      , "box.js"
      , "graph-style.js"
      , "explain.js"
      , "cytoscape.min.js"
      , "cytoscape-dagre.min.js"
      , "cytoscape-expand-collapse.js"
      ]
    step :: Word64 -> Word8 -> Word64
    step h b = (h `xor` fromIntegral b) * 0x100000001b3

css, js :: ByteString
css = "text/css; charset=utf-8"
js = "text/javascript; charset=utf-8"

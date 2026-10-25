{-# LANGUAGE TemplateHaskell #-}

module Quandary.Shell.Assets
  ( asset
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)

-- | Returns the media type and content of the file served at
-- `/assets/NAME`.
asset :: Text -> Maybe (ByteString, ByteString)
asset "style.css" = Just (css, $(embedFile "web/style.css"))
asset "graph.js" = Just (js, $(embedFile "web/graph.js"))
asset "cytoscape.min.js" = Just (js, $(embedFile "web/vendor/cytoscape/cytoscape.min.js"))
asset "cytoscape-dagre.min.js" = Just (js, $(embedFile "web/vendor/cytoscape-dagre/cytoscape-dagre.min.js"))
asset "cytoscape-expand-collapse.js" =
  Just (js, $(embedFile "web/vendor/cytoscape-expand-collapse/cytoscape-expand-collapse.js"))
asset _ = Nothing

css, js :: ByteString
css = "text/css; charset=utf-8"
js = "text/javascript; charset=utf-8"

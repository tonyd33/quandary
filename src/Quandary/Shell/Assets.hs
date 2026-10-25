{-# LANGUAGE TemplateHaskell #-}

module Quandary.Shell.Assets
  ( asset
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)

-- | Returns the script served at `/assets/NAME`.
asset :: Text -> Maybe ByteString
asset "cytoscape.min.js" = Just $(embedFile "web/vendor/cytoscape/cytoscape.min.js")
asset "cytoscape-dagre.min.js" = Just $(embedFile "web/vendor/cytoscape-dagre/cytoscape-dagre.min.js")
asset "cytoscape-expand-collapse.js" = Just $(embedFile "web/vendor/cytoscape-expand-collapse/cytoscape-expand-collapse.js")
asset _ = Nothing

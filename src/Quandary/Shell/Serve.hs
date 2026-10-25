module Quandary.Shell.Serve
  ( app
  , route
  )
where

import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Lucid (Html, renderBS)
import Network.HTTP.Types
import Network.Wai
import Quandary.Core.Types
import Quandary.Shell.Browse
import Quandary.Shell.Render.Html (document, link)
import Quandary.Shell.Resolve (Verb, verbNamed)

app :: Live -> Text -> Int -> Application
app live point port req respond
  | not (hostAllowed port (requestHeaderHost req)) =
      respond (plain status403 "refused: the Host header is not this server's\n")
  | requestMethod req /= methodGet = respond (plain status405 "only GET\n")
  | pathInfo req == ["events"] = respond (events live (since (queryString req)))
  | otherwise = case route point (pathInfo req) (queryString req) of
      Nothing -> respond (plain status404 "not found\n")
      Just r -> do
        d <- page live r
        respond (responseOf r d)

responseOf :: Route -> Drawn -> Response
responseOf r d = case d.shown of
  Shown _ (NotFound _) -> html status404 [] (document r d)
  Shown _ (Redirect to) -> html status303 [(hLocation, encodeUtf8 (link to))] (document r d)
  _ -> html status200 [] (document r d)

plain :: Status -> ByteString -> Response
plain s t = responseLBS s [(hContentType, "text/plain; charset=utf-8")] (Lazy.fromStrict t)

html :: Status -> ResponseHeaders -> Html () -> Response
html s hs h = responseLBS s ((hContentType, "text/html; charset=utf-8") : hs) (renderBS h)

-- | Holds for `127.0.0.1:N` and `localhost:N`, where N is the port served.
hostAllowed :: Int -> Maybe ByteString -> Bool
hostAllowed port host = host `elem` [Just (h <> ":" <> Char8.pack (show port)) | h <- ["127.0.0.1", "localhost"]]

since :: Query -> Maybe Revision
since q = case lookup "since" q of
  Just (Just t) | [(n, "")] <- reads (Char8.unpack t) -> Just (Revision n)
  _ -> Nothing

-- | Send each new revision as a server-sent event.
events :: Live -> Maybe Revision -> Response
events live from = responseStream status200 [(hContentType, "text/event-stream"), (hCacheControl, "no-cache")] $ \write flush -> do
  flush
  start <- maybe (currentRevision live) pure from
  let loop rev = do
        rev'@(Revision n) <- awaitChange live rev
        write (byteString ("data: " <> Char8.pack (show n) <> "\n\n"))
        flush
        loop rev'
  loop start

-- | Returns the route a URL path and query name, `/` the status at the
-- point given.
route :: Text -> [Text] -> Query -> Maybe Route
route point [] _ = Just (StatusAt point)
route _ ["points"] _ = Just Points
route _ ["p", p] _ = Just (StatusAt p)
route _ ("p" : p : "q" : i@(_ : _)) _ = Just (QuestionAt p (Id (Text.intercalate "/" i)))
route _ ("p" : p : "o" : i@(_ : _)) _ = Just (OptionAt p (Text.intercalate "/" i))
route _ ["p", p, "whatif"] q = WhatifAt p <$> traverse supposition q
route _ ["p", p, "go"] q = GoTo p <$> (join (lookup "id" q) >>= decoded)
route _ _ _ = Nothing

supposition :: QueryItem -> Maybe (Verb, Text)
supposition (k, Just v) = do
  verb <- decoded k >>= verbNamed
  t <- decoded v
  pure (verb, t)
supposition (_, Nothing) = Nothing

decoded :: ByteString -> Maybe Text
decoded = either (const Nothing) Just . decodeUtf8'

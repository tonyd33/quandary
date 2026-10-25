module Quandary.Shell.Serve
  ( app
  , route
  )
where

import Control.Monad (join)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (isDigit, isHexDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Lucid (Html, renderBS)
import Network.HTTP.Types
import Network.Wai
import Quandary.Core.Types
import Quandary.Core.View (Dead (..))
import Quandary.Shell.Assets qualified as Assets
import Quandary.Shell.Browse
import Quandary.Shell.Render.Html (document, graphElements, link, matchesHtml, titledDocument)
import Quandary.Shell.Resolve (Verb, verbNamed)
import Quandary.Shell.Storage.Files (Override (..))
import Text.Read (readMaybe)

app :: Live -> Text -> Int -> Application
app live point port req respond
  | not (hostAllowed port (requestHeaderHost req)) =
      respond (plain status403 "refused: the Host header is not this server's\n")
  | requestMethod req /= methodGet && not (sameOrigin req) =
      respond (plain status403 "refused: the Origin header is not this server's\n")
  | requestMethod req == methodPost, ["p", p, "rule"] <- pathInfo req = recordFrom live p req >>= respond
  | requestMethod req /= methodGet = respond (plain status405 "only GET, and POST to record\n")
  | pathInfo req == ["events"] = respond (events live (since (queryString req)))
  | ["assets", name] <- pathInfo req = respond (maybe (plain status404 "not found\n") served (Assets.asset name))
  | ["p", p, "graph", "elements"] <- pathInfo req =
      page live (GraphAt p (dead (queryString req))) >>= respond . elementsOf
  | ["p", p, "find"] <- pathInfo req =
      respond . html status200 . matchesHtml p
        =<< goToMatches live p (fromMaybe "" (join (lookup "q" (queryString req)) >>= decoded))
  | ["p", p, "go"] <- pathInfo req
  , Just t <- join (lookup "id" (queryString req)) >>= decoded =
      goToRoute live p t >>= respond . either (responseOf (titledDocument p ("go to " <> t <> " at " <> p))) redirect
  | otherwise = case route point (pathInfo req) (queryString req) of
      Nothing -> respond (plain status404 "not found\n")
      Just r -> inspectedPage live r >>= respond . responseOf (document r)

-- | Record the suppositions the form posts.
recordFrom :: Live -> Text -> Request -> IO Response
recordFrom live p req =
  recordings . parseQuery . Lazy.toStrict <$> strictRequestBody req >>= \case
    Nothing -> pure (plain status400 "a malformed record form\n")
    Just (rev, override, rs) -> responseOf (titledDocument p ("record at " <> p)) <$> record live p rev override rs

-- | Holds when the request's `Origin` is this server as its `Host` names it.
sameOrigin :: Request -> Bool
sameOrigin req = case (lookup "Origin" (requestHeaders req), requestHeaderHost req) of
  (Just origin, Just host) -> origin == "http://" <> host
  _ -> False

-- | Returns the revision the form was drawn at, whether it overrides the
-- edit lock, and each supposition with its reason, in order: a verb and
-- id, then a reason.
recordings :: Query -> Maybe (Revision, Override, [(Verb, Text, Text)])
recordings q = do
  rev <- revisionOf =<< join (lookup "revision" q)
  rs <- suppositions [item | item@(k, _) <- q, k `notElem` ["revision", "force"]]
  pure (rev, override, rs)
  where
    override = if isJust (lookup "force" q) then OverrideLock else KeepLock

suppositions :: Query -> Maybe [(Verb, Text, Text)]
suppositions [] = Just []
suppositions (item : ("reason", Just reason) : more) = do
  (v, t) <- supposition item
  one <- (,,) v t <$> decoded reason
  (one :) <$> suppositions more
suppositions _ = Nothing

revisionOf :: ByteString -> Maybe Revision
revisionOf t = Revision <$> readMaybe (Char8.unpack t)

responseOf :: (Drawn -> Html ()) -> Drawn -> Response
responseOf render d = html status (render d)
  where
    status = case d.shown of
      Shown _ (NotFound _) -> status404
      _ -> status200

redirect :: Route -> Response
redirect r = responseLBS status303 [(hContentType, "text/html; charset=utf-8"), (hLocation, encodeUtf8 (link r))] ""

served :: (ByteString, ByteString) -> Response
served (mediaType, content) =
  responseLBS status200 [(hContentType, mediaType), (hCacheControl, "no-cache")] (Lazy.fromStrict content)

-- | Returns the graph's elements as JSON; not found for an unknown point,
-- and a conflict when the space has problems.
elementsOf :: Drawn -> Response
elementsOf d = case d.shown of
  Shown _ (GraphPage p _ g) -> responseLBS status200 [(hContentType, "application/json")] (Aeson.encode (graphElements p g))
  Shown _ _ -> plain status404 "not found\n"
  Problems _ -> plain status409 "the space has problems\n"

plain :: Status -> ByteString -> Response
plain s t = responseLBS s [(hContentType, "text/plain; charset=utf-8")] (Lazy.fromStrict t)

html :: Status -> Html () -> Response
html s h = responseLBS s [(hContentType, "text/html; charset=utf-8")] (renderBS h)

-- | Holds for `localhost:N` or an IP address with `:N`, where N is the port
-- served. Every other name is refused.
hostAllowed :: Int -> Maybe ByteString -> Bool
hostAllowed port (Just host)
  | Just name <- Char8.stripSuffix (":" <> Char8.pack (show port)) host = name == "localhost" || ipLiteral name
hostAllowed _ _ = False

-- | Holds for a dotted IPv4 address or a bracketed IPv6 one.
ipLiteral :: ByteString -> Bool
ipLiteral name = ipv4 || ipv6
  where
    ipv4 = case Char8.split '.' name of
      parts@[_, _, _, _] -> all octet parts
      _ -> False
    octet p = not (Char8.null p) && Char8.all isDigit p && Char8.length p <= 3 && read (Char8.unpack p) <= (255 :: Int)
    ipv6 =
      Char8.length name > 2
        && Char8.head name == '['
        && Char8.last name == ']'
        && Char8.all ipv6Char (Char8.init (Char8.tail name))
    ipv6Char c = isHexDigit c || c == ':' || c == '.'

since :: Query -> Maybe Revision
since q = lookup "since" q >>= (>>= revisionOf)

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
route _ ["p", p, "rulings"] _ = Just (RulingsAt p)
route _ ["p", p, "search"] q = SearchAt p <$> maybe (Just "") decoded (join (lookup "q" q))
route _ ["p", p, "whatif"] q = WhatifAt p <$> traverse supposition q
route _ ["p", p, "graph"] q = Just (GraphAt p (dead q))
route _ ["p", p, "record"] q = RecordAt p <$> traverse supposition q
route _ ["p", p, "box"] q = BoxAt p (dead q) <$> played q
route _ _ _ = Nothing

-- | Returns the suppositions the query plays, in order, when it holds
-- `play`.
played :: Query -> Maybe (Maybe [(Verb, Text)])
played q
  | isJust (lookup "play" q) = Just <$> traverse supposition [i | i@(k, _) <- q, k `notElem` ["play", "all"]]
  | otherwise = Just Nothing

-- | Returns whether the query asks for dead parts too, with `all`.
dead :: Query -> Dead
dead q
  | isJust (lookup "all" q) = ShowDead
  | otherwise = HideDead

supposition :: QueryItem -> Maybe (Verb, Text)
supposition (k, Just v) = do
  verb <- decoded k >>= verbNamed
  t <- decoded v
  pure (verb, t)
supposition (_, Nothing) = Nothing

decoded :: ByteString -> Maybe Text
decoded = either (const Nothing) Just . decodeUtf8'

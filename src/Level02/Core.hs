{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType respBody =
  let headers = [("Content-Type", renderContentType contentType)]
   in responseLBS status headers respBody

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment =
  AddRq <$> mkTopic topic <*> mkCommentText (lazyByteStringToStrictText comment)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic =
  ViewRq <$> mkTopic topic

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopic = resp400 PlainText "Empty Topic"
mkErrorResponse EmptyCommentText = resp400 PlainText "Empty Comment Text"
mkErrorResponse UnknownRequest = resp404 PlainText "Unknown Request"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (requestMethod req, pathInfo req) of
    ("POST", [topic, "add"]) -> mkAddRequest topic <$> strictRequestBody req
    ("GET", [topic, "view"]) -> pure $ mkViewRequest topic
    ("GET", ["list"]) -> pure mkListRequest
    _ -> pure $ Left UnknownRequest

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _topic _commentText) = Right $ resp200 PlainText "Add not implemented yet"
handleRequest (ViewRq _topic) = Right $ resp200 PlainText "View not implemented yet"
handleRequest ListRq  = Right $ resp200 PlainText "List not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app req cb = do
  result <- mkRequest req
  case result >>= handleRequest of
    Right resp -> cb resp
    Left err -> cb $ mkErrorResponse err

runApp :: IO ()
runApp = run 3000 app

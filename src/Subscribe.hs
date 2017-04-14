{-# LANGUAGE OverloadedStrings #-}

module Subscribe
    (
      renewalRequest,
      subscribeEvent,
      unSubscribeEvent
    ) where

import           AVTransport
import           Data.List.Split
import           Data.Maybe
import           Data.Time.Clock
import           Data.UUID
import           Network.HTTP
import           Network.UPnP
import           Network.URI



subRequest:: AVService -> URI -> Request String
subRequest (dev, sev) cburi = Request (getEventSubURL sev) (Custom "SUBSCRIBE") subscribeHead ""
  where
    subscribeHead = [ mkHeader HdrHost $ uriToAuthorityString $ getUpnpURI dev ,
                      mkHeader (HdrCustom "CALLBACK") $ "<" ++ show cburi ++ ">" ,
                      mkHeader (HdrCustom "NT") "upnp:event"]

unSubRequest::AVService -> UUID -> Request String
unSubRequest (dev, sev) renewalID = Request (getEventSubURL sev) (Custom "SUBSCRIBE") subscribeHead ""
  where
    subscribeHead = [ mkHeader HdrHost $ uriToAuthorityString $ getUpnpURI dev ,
                      mkHeader (HdrCustom "SID") $ "uuid:" ++ toString renewalID]


renewalRequest:: AVService -> UUID -> Request String
renewalRequest (dev, sev) renewalID = Request (getEventSubURL sev) (Custom "SUBSCRIBE") subscribeHead ""
  where
    subscribeHead = [ mkHeader HdrHost $ uriToAuthorityString $ getUpnpURI dev ,
                      mkHeader (HdrCustom "SID") $ "uuid:" ++ toString renewalID ]

subscribeEvent :: AVService -> URI -> IO (UUID, DiffTime)
subscribeEvent (dev, sev) cburi = do
    Right rsp <- simpleHTTP req
    return (getSid rsp, getTimeout rsp)
  where
    req = subRequest (dev, sev) cburi
    getSid rsp= fromJust.fromString.last.splitOn ":".fromJust.lookupHeader (HdrCustom "Sid") $ rspHeaders rsp
    getTimeout rsp = secondsToDiffTime.read.last.splitOn "-".fromJust.lookupHeader (HdrCustom "Timeout") $ rspHeaders rsp

unSubscribeEvent :: AVService -> (UUID, DiffTime) -> IO ()
unSubscribeEvent a (uuid, _) = do
  _ <- simpleHTTP $ unSubRequest a uuid
  return ()

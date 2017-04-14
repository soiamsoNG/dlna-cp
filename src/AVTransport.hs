{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AVTransport
    (
      actionPlay,
      actionSetAVTransportURI,
      actionSetNextAVTransportURI,
      actionGetPositionInfo,
      -- actionGetDeviceCapabilities,
      avTrasportStateLoop,
      AVService,
      getFullAVURI,
      ThreadTimeoutException
    ) where

import           Control.Arrow.ArrowTree  ((//>))
import           Control.Concurrent       (threadDelay, Chan, readChan)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar  (MVar, readMVar, takeMVar, isEmptyMVar, putMVar)
import           Control.Exception
import           Data.Default
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import           Data.Typeable            (Typeable)
import           Network.HTTP
import           Network.Stream           (Result)
import           Network.UPnP
import           Network.URI
import           Text.XML
import           Text.XML.HXT.Core        (getText, hasName, readString, runX)
import           Text.XML.Writer

type AVService = (Upnp Device, Upnp Service)

getFullAVURI::AVService -> URI
getFullAVURI (dev, sev) = getControlURL sev `relativeTo` getUpnpURI dev

actionPlay::AVService -> IO (Result (Response String))
actionPlay a = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#Play\"", mkHeader HdrContentLength (show $ length body)]
    soapbody = soap () $
          element (Name "Play" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $ do
            element "InstanceID" (0::Int)
            element "Speed" (1::Int)

actionSetAVTransportURI:: AVService -> URI -> IO (Result (Response String))
actionSetAVTransportURI a ms = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#SetAVTransportURI\"", mkHeader HdrContentLength (show $ length body)]
    soapbody = soap () $
      element (Name "SetAVTransportURI" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $ do
        element "InstanceID" (0::Int)
        element "CurrentURI" $ T.pack $ show ms
        element "CurrentURIMetaData" ()

actionSetNextAVTransportURI:: AVService -> URI -> IO (Result (Response String))
actionSetNextAVTransportURI a ms = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#SetNextAVTransportURI\"", mkHeader HdrContentLength (show $ length body)]
    soapbody = soap () $
      element (Name "SetNextAVTransportURI" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $ do
        element "InstanceID" (0::Int)
        element "NextURI" $ T.pack $ show ms
        element "NextURIMetaData" ()

actionGetPositionInfo:: AVService -> IO (Result (Response String))
actionGetPositionInfo a = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#GetPositionInfo\"", mkHeader HdrContentLength (show $ length body)]
    soapbody = soap () $
      element (Name "GetPositionInfo" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $
        element "InstanceID" (0::Int)

-- actionGetDeviceCapabilities:: AVService -> IO (Result (Response String))
-- actionGetDeviceCapabilities a = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
--   where
--     body = TL.unpack $ renderText def soapbody
--     soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#GetDeviceCapabilities\"", mkHeader HdrContentLength (show $ length body)]
--     soapbody = soap () $
--       element (Name "GetDeviceCapabilities" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $
--         element "InstanceID" (0::Int)

data ThreadTimeoutException = ThreadTimeoutException
     deriving (Show, Typeable)

instance Exception ThreadTimeoutException



reltimeTicking::MVar String -> Chan URI -> AVService -> IO()
reltimeTicking trackstate tracklist as = do
      Right rsp <- actionGetPositionInfo as
      -- print $ rspBody rsp
      [turi] <- runX $ readString [] (rspBody rsp) //> hasName "TrackURI" //> getText
      [rtime] <- runX $ readString [] (rspBody rsp) //> hasName "RelTime" //> getText
      ts <- isEmptyMVar trackstate
      if ts then putMVar trackstate turi
        else do
          a <- readMVar trackstate
          if a == turi then return ()
            else takeMVar trackstate >> async (trackChanged tracklist as) >> return ()
      print rtime
      threadDelay 1000000
      reltimeTicking trackstate tracklist as
    where
      trackChanged t a = do
          nextURI <- readChan t  -- there will be a infinity waiting when Chan empty. will always leak one thread.
          Right _ <- actionSetNextAVTransportURI a nextURI
          putStrLn $ "set next to " ++ show nextURI
          return ()


avTrasportStateLoop:: MVar String -> MVar String -> Chan URI -> AVService -> IO()
avTrasportStateLoop playstate trackstate tracklist as = do
  Left ps <- race (readMVar playstate) (threadDelay 30000000 >> throw ThreadTimeoutException)
  putStrLn $ "avTrasportStateLoop Redispatch: " ++ ps
  case ps of
    "PLAYING" -> takeMVar playstate >> race_ (readMVar playstate) (reltimeTicking trackstate tracklist as) >> avTrasportStateLoop playstate trackstate tracklist as
    "TRANSITIONING" -> takeMVar playstate >> avTrasportStateLoop playstate trackstate tracklist as
    "PAUSED_PLAYBACK" -> takeMVar playstate >> avTrasportStateLoop playstate trackstate tracklist as
    _ -> return ()

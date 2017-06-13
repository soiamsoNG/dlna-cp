{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AVTransport
    (
      actionPlay,
      actionStop,
      actionNext,
      actionSetAVTransportURI,
      actionSetNextAVTransportURI,
      actionGetPositionInfo,
      -- actionGetDeviceCapabilities,
      setNextAVTransportURI,
      avTrasportStateLoop,
      AVService,
      getFullAVURI,
      ThreadTimeoutException
    ) where

import           Control.Arrow.ArrowTree  ((//>))
import           Control.Concurrent       (Chan, readChan, threadDelay,
                                           unGetChan)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar  (MVar, isEmptyMVar, putMVar, readMVar,
                                           takeMVar)
import           Control.Exception
import           Control.Monad            (when)
import           Data.Default
import           Data.Maybe               (fromMaybe)
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

actionStop::AVService -> IO (Result (Response String))
actionStop a = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#Stop\"", mkHeader HdrContentLength (show $ length body)]
    soapbody = soap () $
          element (Name "Stop" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $
            element "InstanceID" (0::Int)

actionNext::AVService -> IO (Result (Response String))
actionNext a = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#Next\"", mkHeader HdrContentLength (show $ length body)]
    soapbody = soap () $
          element (Name "Next" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $
            element "InstanceID" (0::Int)

emptyDIDLLite :: XML
emptyDIDLLite = toXML ("<DIDL-Lite></DIDL-Lite>"::T.Text)


actionSetAVTransportURI:: AVService -> URI -> IO (Result (Response String))
actionSetAVTransportURI a ms = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#SetAVTransportURI\""
                      ,mkHeader HdrContentLength (show $ length body)
                     ]
    soapbody = soap () $
      element (Name "SetAVTransportURI" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $ do
        element "InstanceID" (0::Int)
        element "CurrentURI" $ T.pack $ show ms
        element "CurrentURIMetaData" emptyDIDLLite

actionSetNextAVTransportURI:: AVService -> URI -> IO (Result (Response String))
actionSetNextAVTransportURI a ms = simpleHTTP $ Request (getFullAVURI a) POST soapactionhead body
  where
    body = TL.unpack $ renderText def soapbody
    soapactionhead = [mkHeader (HdrCustom "SOAPACTION") "\"urn:schemas-upnp-org:service:AVTransport:1#SetNextAVTransportURI\"", mkHeader HdrContentLength (show $ length body)]
    soapbody = soap () $
      element (Name "SetNextAVTransportURI" (Just "urn:schemas-upnp-org:service:AVTransport:1") (Just "u")) $ do
        element "InstanceID" (0::Int)
        element "NextURI" $ T.pack $ show ms
        element "NextURIMetaData" emptyDIDLLite

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
reltimeTicking trackstate tracklist as = handle
  (\(e::IOException) -> if show e == "connect: does not exist (Connection refused)"
      then do
        putStrLn "Connection refused, Retry"
        threadDelay 5000000
        reltimeTicking trackstate tracklist as
      else throw e) $ do
      Right rsp <- actionGetPositionInfo as
      when (rspBody rsp /= "") $ do
        turis <- runX $ readString [] (rspBody rsp) //> hasName "TrackURI" //> getText
        [rtime] <- runX $ readString [] (rspBody rsp) //> hasName "RelTime" //> getText
        let turi = case turis of
                    [a] -> a
                    _   -> ""
        ts <- isEmptyMVar trackstate
        if ts then putMVar trackstate turi
          else do
            a <- readMVar trackstate
            -- if a not equal turi we know that the track already change by the player
            -- then we need to drop the head of the tracklist and then setNextAVTransportURI
            when (a /= turi && turi /= "") $
              takeMVar trackstate >> do
                nextURI <- readChan tracklist  -- there will be a infinity waiting when Chan empty.
                unGetChan tracklist nextURI
                if nextURI == fromMaybe (error "Can not parse the URI") (parseURI turi)
                  then readChan tracklist >> async (setNextAVTransportURI tracklist as) >> return ()
                  else async (setNextAVTransportURI tracklist as) >> return ()
        print rtime
        -- when (rtime == "00:00:00") $
        --   print $ rspBody rsp
      threadDelay 5000000
      reltimeTicking trackstate tracklist as

setNextAVTransportURI :: Chan URI -> AVService -> IO ()
setNextAVTransportURI t a = do
    nextURI <- readChan t  -- there will be a infinity waiting when Chan empty. will always leak one thread.
    unGetChan t nextURI
    Right _ <- actionSetNextAVTransportURI a nextURI
    putStrLn $ "Set next to " ++ show nextURI
    return ()



avTrasportStateLoop:: MVar String -> MVar String -> Chan URI -> AVService -> IO()
avTrasportStateLoop playstate trackstate tracklist as = do
    Left ps <- race (readMVar playstate) (threadDelay 30000000 >> throw ThreadTimeoutException)
    putStrLn $ "avTrasportStateLoop Redispatch: " ++ ps
    case ps of
      "PLAYING" -> takeMVar playstate >> race_ (readMVar playstate) (reltimeTicking trackstate tracklist as) >> avTrasportStateLoop playstate trackstate tracklist as
      "TRANSITIONING" -> takeMVar playstate >> avTrasportStateLoop playstate trackstate tracklist as
      "PAUSED_PLAYBACK" -> takeMVar playstate >> avTrasportStateLoop playstate trackstate tracklist as
      "MANUAL_NEXT" -> takeMVar playstate >> async (readChan tracklist >>= actionSetAVTransportURI as) >> avTrasportStateLoop playstate trackstate tracklist as
      _  -> return ()

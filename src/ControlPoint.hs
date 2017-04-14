{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           AVTransport
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception        (handle)
import           Control.Monad            (when)
import           Data.Maybe
import           EventServer
import           FileServer
import           Network
import           Network.HTTP
import           Network.SSDP
import           Network.UPnP
import           Network.URI
import           Subscribe
import           System.Directory
import System.Console.GetOpt
import System.Environment(getArgs)


parseHostURI::String -> URI
parseHostURI a = fromMaybe
                    (error "Should be a URI like http://192.168.1.107:9000/")
                    (parseURI a)


getEventServerURI::URI -> URI
getEventServerURI a = a{uriAuthority = Just new}
  where
    new = ua{uriPort = ":9090"}
    ua = fromJust $ uriAuthority a

data Options = Options {
  optHost     :: Maybe URI
  , optFileDir :: Maybe FilePath
} deriving Show

defaultOptions::Options
defaultOptions = Options{
    optHost = Nothing
  , optFileDir = Nothing
}

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h']     ["host"]
        (ReqArg (\h opts -> opts{optHost=Just (parseHostURI h)}) "HOST")
        "Host Http Address"
    , Option ['d']     ["filedir"]
        (ReqArg (\d opts -> opts{optFileDir=Just d}) "DIR")
        "Directory which include Media "
    ]


getURIPort::URI -> Int
getURIPort = read.tail.uriPort.fromJust.uriAuthority

toPlaypath::URI -> FilePath -> URI
toPlaypath hostURI fp = fromMaybe
                (error "Maybe miss tailling / at Host URI")
                (parseURI $ show hostURI ++ urlEncode fp)


filterAVTransportService :: Upnp Device -> Upnp Service
filterAVTransportService dev = head $ filter isAVTransportService $ getServiceList dev
  where
    isAVTransportService d = Network.UPnP.serviceType (fromJust $ getServiceType d) == "AVTransport"


getAVTransportOfFirstDevice :: IO (Upnp Device, Upnp Service)
getAVTransportOfFirstDevice = do
  -- SSDP search
  let ssdp = ssdpSearch (UrnService "schemas-upnp-org" "AVTransport" "1")
                        Nothing Nothing
  (res,_) <- sendSearch ssdp
  let ((_, notify):_) = filter (hasHeader "LOCATION" . snd) res
  -- get description
  mdev <- requestDeviceDescription notify
  case mdev of
    Right dev -> return (dev, filterAVTransportService dev)
    Left err  -> error err


controlPointOpts :: IO (Options, [String])
controlPointOpts = do
  argv <- getArgs
  case getOpt Permute options argv of
     (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."


main :: IO ()
main = do

  (opts,_) <- controlPointOpts

  let hostURI = fromMaybe (error "Need set host address by -h") (optHost opts)
  let mediaPath = fromMaybe (error "Need set file dirtry to Media by -d") (optFileDir opts)
  let eventServerURI = getEventServerURI hostURI

  playstate <- newEmptyMVar
  trackstate <- newEmptyMVar
  tracklist <- newChan

  a_fs <- async $ runFileServer (getURIPort hostURI) mediaPath
  a_event <- async $ runEventServer playstate (getURIPort eventServerURI)
  a_dev <- async $ withSocketsDo getAVTransportOfFirstDevice
  (dev, sev) <- wait a_dev
  ut <- subscribeEvent (dev, sev) eventServerURI

  fps <- listDirectory mediaPath
  writeList2Chan tracklist $ map (toPlaypath hostURI) fps

  when (length fps == 1) $ do
        a1 <- readChan tracklist
        _ <- actionSetAVTransportURI (dev,sev) a1
        _ <- actionPlay (dev,sev)
        return ()

  when (length fps > 1) $ do
        a1 <- readChan tracklist
        _ <- actionSetAVTransportURI (dev,sev) a1
        a2 <- readChan tracklist
        _ <- actionSetNextAVTransportURI (dev,sev) a2
        _ <- actionPlay (dev,sev)
        return ()

  a_loop <- async $ handle (\(_::ThreadTimeoutException) -> putStrLn "avTrasportStateLoop timeout") $
              avTrasportStateLoop playstate trackstate tracklist (dev,sev)
  wait a_loop
  putStrLn "EventLoopLeave"
  unSubscribeEvent (dev, sev) ut
  putStrLn "UnSubscribeEvent Success"
  cancel a_fs
  putStrLn "FileServerLeave"
  cancel a_event
  putStrLn "EventServerLeave"
  return ()

{-# LANGUAGE OverloadedStrings #-}

module EventServer
    (
      runEventServer,
    ) where

import           Control.Arrow            ((>>>))
import           Control.Arrow.ArrowTree  ((//>))
import           Control.Concurrent.MVar
import           Control.Monad            (unless)
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, requestBody,
                                           responseLBS)
import           Network.Wai.Handler.Warp (Port, run)
import           Text.XML.HXT.Core        (getAttrValue, getText, hasName,
                                           readString, runX)

app :: MVar String ->  Application
app playstate req f = do
    result <- requestBody req
    -- print result
    ch <- runX $ readString [] (T.unpack $ decodeUtf8 result) //> hasName "LastChange" //> getText
    case ch of
      [change] -> do
          [state] <- runX $ readString [] change //> hasName "TransportState" >>> getAttrValue "val"
          unless (state == "") $ do
              em <- isEmptyMVar playstate
              if em
                then putMVar playstate state
                else do
                  _ <- swapMVar playstate state
                  return ()
              return ()
          f $ responseLBS status200 [] ""

      _ -> f $ responseLBS status200 [] ""

runEventServer :: MVar String -> Port -> IO()
runEventServer playstate p = run p $ app playstate

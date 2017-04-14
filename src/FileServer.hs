{-# LANGUAGE OverloadedStrings #-}

module FileServer
    (
      runFileServer
    ) where

import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Directory

runFileServer :: Port -> FilePath -> IO()
runFileServer p fp = do
  dfe <- doesFileExist fp
  dde <- doesDirectoryExist fp
  let d = if dfe || dde
            then fp
            else error "filepath not exist"
  run p $ staticApp $ defaultFileServerSettings d

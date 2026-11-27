module Main (main) where

import Mdmaster.Web.Config (loadConfig)
import Mdmaster.Web.Server (runServer)

main :: IO ()
main = loadConfig >>= runServer

module Main where

import Protolude

import Data.String (String)
import System.Process (callProcess)
import Data.Aeson.Lens
import Control.Lens
import Data.Text (unpack)
import qualified System.IO.Streams as Streams
import Control.Monad.Trans.Maybe
import Control.Error.Util

player :: String
player = "mpv"

options :: [String]
options = ["--no-terminal"
          ,"--volume=70"
          ]

main :: IO ()
main = run $
  read >> read
  >>= play . (: options) . unpack . view (key "link" . _String)
  where read = hoistMaybe =<< (liftIO $ Streams.read Streams.stdin)
        play = liftIO . callProcess player
        run a = runMaybeT a >> return ()

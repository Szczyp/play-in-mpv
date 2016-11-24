module Main where

import Protolude hiding (drop)

import Data.String (String)
import System.Process (callProcess)
import Data.Aeson.Lens
import Control.Lens
import Data.Text (unpack)
import qualified System.IO.Streams as Streams
import Control.Monad.Trans.Maybe
import Control.Error.Util
import Data.ByteString (drop)
import System.IO

player :: String
player = "mpv"

defaultOptions :: [String]
defaultOptions = ["--no-terminal"
                 ,"--volume=70"
                 ]

main :: IO ()
main = run $ play . options =<< read
  where read = hoistMaybe =<< liftIO (Streams.read Streams.stdin)
        play = liftIO . callProcess player
        options = (: defaultOptions) . unpack . view (key "link" . _String) . drop 4
        run a = do
          hSetBuffering stdin LineBuffering
          void $ runMaybeT a

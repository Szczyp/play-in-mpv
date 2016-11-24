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
import qualified Data.ByteString as ByteString
import System.IO

player :: String
player = "mpv"

defaultOptions :: [String]
defaultOptions = ["--no-terminal"
                 ,"--volume=70"
                 ]

main :: IO ()
main = do
  bufferMode <- hGetBuffering stdin
  run $ case bufferMode of
          LineBuffering -> ByteString.drop 4 <$> read
          _             -> read >> read
    >>= play . options
  where read = hoistMaybe =<< liftIO (Streams.read Streams.stdin)
        play = liftIO . callProcess player
        options = (: defaultOptions) . unpack . view (key "link" . _String)
        run = void . runMaybeT

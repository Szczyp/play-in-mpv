{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude hiding (StateT, for, fromStrict, stdin)

import Control.Error.Util        (hoistMaybe)
import Control.Lens              (view)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Aeson.Lens           (key, _String)
import Data.Binary.Get           (getInt32host, runGet)
import Data.ByteString           (ByteString, pack)
import Data.ByteString.Lazy      (fromStrict)
import Data.Text                 (Text, unpack, unwords)
import Pipes                     (Producer, for, runEffect)
import Pipes.ByteString          (drawByte, stdin)
import Pipes.Parse               (StateT, parsed_)
import System.Process            (callCommand)


player :: Text
player = "mpv"

defaultOptions :: [Text]
defaultOptions = ["--no-terminal"
                 ,"--volume=80"
                 ,"--ytdl-format=\"bestvideo[height<=?1080]+bestaudio\""
                 ,"--ytdl-raw-options=mark-watched=,cookie=~/Downloads/cookies"
                 ]

readUrl :: Monad m => MaybeT (StateT (Producer ByteString m x) m) Text
readUrl = do
  n <- runGet getInt32host . fromStrict <$> drawBytes 4
  view (key "link" . _String) <$> drawBytes (fromIntegral n)
  where drawBytes n = do
          draws <- lift $ replicateM n drawByte
          pack <$> hoistMaybe (sequence draws)

main :: IO ()
main = void $ runEffect $ for urls $ play . options
  where urls = parsed_ (runMaybeT readUrl) stdin
        play = lift . spawn . unpack . unwords . (player :)
        options url = defaultOptions <> ["\"" <> url <> "\""]
        spawn = void . forkIO . callCommand


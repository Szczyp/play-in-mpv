{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude hiding (StateT, fromStrict, for, stdin)

import Control.Error.Util (hoistMaybe)
import Control.Lens (view)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Aeson.Lens (key, _String)
import Data.Binary.Get (runGet, getInt32host)
import Data.ByteString           (ByteString, pack)
import Data.ByteString.Lazy      (fromStrict)
import Data.Text                 (Text, unpack, unwords)
import Pipes (Producer, runEffect, for)
import Pipes.ByteString          (drawByte, stdin)
import Pipes.Parse (StateT, parsed_)
import System.Process            (spawnCommand)

player :: Text
player = "mpv"

defaultOptions :: [Text]
defaultOptions = ["--no-terminal"
                 ,"--volume=80"
                 ,"--ytdl-format=\"bestvideo[height<=?1080]+bestaudio\""
                 ,"--ytdl-raw-options=mark-watched=,cookie=~/Downloads/cookies"
                 ]

drawBytes :: Monad m => Int -> MaybeT (StateT (Producer ByteString m x) m) ByteString
drawBytes n = do
  draws <- lift $ replicateM n drawByte
  pack <$> hoistMaybe (sequence draws)

readUrl :: Monad m => MaybeT (StateT (Producer ByteString m x) m) Text
readUrl = do
  n <- runGet getInt32host . fromStrict <$> drawBytes 4
  view (key "link" . _String) <$> drawBytes (fromIntegral n)

main :: IO ()
main = void $ runEffect $ for urls $ void . play . options
  where urls = parsed_ (runMaybeT readUrl) stdin
        play = lift . spawnCommand . unpack . unwords . (player :)
        options url = defaultOptions <> ["\"" <> url <> "\""]

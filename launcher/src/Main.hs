{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude hiding (StateT, for, fromStrict, stdin, stdout)

import Control.Error.Util
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import Data.Binary
import Data.ByteString           (pack)
import Data.ByteString.Lazy      (fromStrict)
import Data.Text                 hiding (pack)
import Pipes
import Pipes.ByteString          hiding (pack, unpack, unwords)
import Pipes.Parse
import System.Process            (callCommand)

player :: Text
player = "mpv"

defaultOptions :: [Text]
defaultOptions = ["--no-terminal"
                 ,"--volume 80"
                 ,"--ytdl-format 'bestvideo[height<=?1080]+bestaudio'"
                 ,"--ytdl-raw-options mark-watched=,cookie=~/Downloads/cookies"
                 ]

drawBytes :: Monad m => Int -> MaybeT (StateT (Producer ByteString m x) m) ByteString
drawBytes n = do
  draws <- lift $ replicateM n drawByte
  pack <$> (hoistMaybe $ sequence draws)

readUrl :: Monad m => MaybeT (StateT (Producer ByteString m x) m) Text
readUrl = do
  n <- decode @Int32 . fromStrict <$> drawBytes 4
  view (key "link" . _String) <$> drawBytes (fromIntegral n)

main :: IO ()
main = void $ runEffect $ for urls $ play . options
  where urls = parsed_ (runMaybeT readUrl) stdin
        play = lift . callCommand . unpack . unwords . (player :)
        options url = defaultOptions <> ["'" <> url <> "'"]

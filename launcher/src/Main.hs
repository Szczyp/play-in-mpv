module Main where

import Protolude hiding (StateT, for, hush, list, stdin, to, (&))

import Control.Error.Util
import Control.Lens                     hiding (index)
import Control.Monad.Trans.Maybe        (MaybeT, runMaybeT)
import Data.Aeson.Lens
import Data.Attoparsec.ByteString       (parseOnly)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Binary.Get                  (getInt32host, runGet)
import Data.ByteString                  (pack)
import Data.List                        (lookup)
import Data.Map.Strict                  (fromList)
import Data.Text                        (unwords)
import Pipes                            (Producer, for, runEffect)
import Pipes.ByteString                 (drawByte, stdin)
import Pipes.Parse                      (StateT, parsed_)
import System.Process                   (callCommand)
import URI.ByteString


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
  n <- runGet getInt32host . toS <$> drawBytes 4
  view (key "link" . _String) <$> drawBytes (fromIntegral n)
  where drawBytes n = replicateM n (lift drawByte)
                      >>= hoistMaybe . sequence
                      <&> pack

optionHandlers :: Map ByteString (URI -> Maybe [Text])
optionHandlers = fromList [("www.youtube.com", youtubeOptions)]

youtubeOptions :: URI -> Maybe [Text]
youtubeOptions uri = do
  let query = uri ^. queryL . queryPairsL
  list <- query & lookup "list"
  index <- lookup "index" query >>= hush . parseOnly decimal :: Maybe Int
  let url = uri
            & queryL . queryPairsL .~ [("list", list)]
            & pathL .~ "/playlist"
            & serializeURIRef'
  return $ defaultOptions |> "--playlist-start=" <> show (index - 1) |> toS url

specialOptions :: Text -> Maybe [Text]
specialOptions url = do
  uri <- hush $ parseURI strictURIParserOptions (toS url)
  host <- uri ^? authorityL . _Just . authorityHostL . hostBSL
  optionHandlers ^. at host . _Just . to ($ uri)

main :: IO ()
main = void $ runEffect $ for urls $ play . options
  where urls = parsed_ (runMaybeT readUrl) stdin
        play = lift . spawn . toS . unwords . cons player
        options url = specialOptions url ?: defaultOptions |> url
        spawn = void . forkIO . callCommand


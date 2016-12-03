{-# LANGUAGE DeriveGeneric, NamedFieldPuns #-}
module Main where

import Protolude hiding (StateT, for, hush, list, stdin, to, tryIO, (&))

import Control.Error.Util
import Control.Lens                     hiding (index, each)
import Control.Monad.Morph
import Control.Monad.Trans.Maybe        (MaybeT, runMaybeT)
import Data.Aeson.Lens
import Data.Attoparsec.ByteString       (parseOnly)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Binary.Get                  (getInt32host, runGet)
import Data.ByteString                  (pack)
import Data.List                        (lookup)
import Data.Text                        (unwords)
import Data.Yaml
import Pipes                            (Producer, for, runEffect, each)
import Pipes.ByteString                 (drawByte, stdin)
import Pipes.Parse                      (StateT, parsed_)
import System.Directory
import System.FilePath
import System.IO.Error
import System.Process                   (callCommand)
import URI.ByteString

import qualified Data.Map.Strict as Map

newtype App a = App (ReaderT Config (ExceptT AppError Identity) a)

data AppError

data Config = Config { options :: Map Text Text
                     , flags   :: [Text]
                     , player  :: Text }
              deriving (Show, Generic)
instance FromJSON Config

optionsL :: Lens' Config (Map Text Text)
optionsL = lens options (\c a -> c { options = a })

flagsL :: Lens' Config [Text]
flagsL = lens flags (\c a -> c { flags = a })

playerL :: Lens' Config Text
playerL = lens player (\c a -> c { player = a })

wrapInQuotes :: (Semigroup a, IsString a) => a -> a
wrapInQuotes str = "\"" <> str <> "\""

baseOptions :: Reader Config [Text]
baseOptions = do
  Config {options, flags} <- ask
  return $ map ("--" <>) flags
    <> ifoldMap (\k v -> ["--" <> k <> " " <> wrapInQuotes v]) options

readUrl :: Monad m => MaybeT (StateT (Producer ByteString m x) m) Text
readUrl = do
  n <- runGet getInt32host . toS <$> drawBytes 4
  view (key "link" . _String) <$> drawBytes (fromIntegral n)
  where drawBytes n = replicateM n (lift drawByte)
                      >>= hoistMaybe . sequence
                      <&> pack

optionHandlers :: Map ByteString (URI -> ReaderT Config Maybe [Text])
optionHandlers = Map.fromList [("www.youtube.com", youtubeOptions)]

youtubeOptions :: URI -> (ReaderT Config Maybe) [Text]
youtubeOptions uri = do
  let query = uri ^. queryL . queryPairsL
  list <- lift $ lookup "list" query
  index <- lift $ hush . parseOnly decimal =<< lookup "index" query
  let url = uri
            & queryL . queryPairsL .~ [("list", list)]
            & pathL .~ "/playlist"
            & serializeURIRef'
  options <- hoist generalize baseOptions
  return $ options |> "--playlist-start=" <> show (index - (1 :: Int)) |> toS url

specialOptions :: Text -> ReaderT Config Maybe [Text]
specialOptions url = do
  uri <- lift $ hush $ parseURI strictURIParserOptions (toS url)
  host <- lift $ uri ^? authorityL . _Just . authorityHostL . hostBSL
  options <- lift $ optionHandlers ^. at host
  options uri

readConfig :: ExceptT SomeException IO Config
readConfig = do
  dir <- syncIO $ getAppUserDataDirectory "play_in_mpv"
  decodeFile (dir </> "play_in_mpv.config.yaml") !? throw "can't parse config"
  where throw = toException . userError

playCommand :: Text -> Reader Config Text
playCommand url = do
  bo <- baseOptions
  so <- runReaderT (specialOptions url) <$> ask
  player <- asks player
  return $ unwords $ cons player (so ?: bo |> wrapInQuotes url)

spawnPlayer :: Text -> IO ()
spawnPlayer = void . forkIO . callCommand . toS

urlStream :: MonadIO m => Producer Text m (Producer ByteString m ())
urlStream = parsed_ (runMaybeT readUrl) stdin

app :: Text -> ReaderT Config IO ()
app = lift . spawnPlayer <=< hoist generalize . playCommand

runApp :: Config -> IO ()
runApp config = void $ runEffect $ for urlStream
                $ lift . (`runReaderT` config) . app

main :: IO ()
main = runExceptT readConfig >>= either print runApp

testStream :: MonadIO m => Producer Text m (Producer ByteString m ())
testStream = parsed_ (runMaybeT readUrl)
  (each ["6\NUL\NUL\NUL{\"link\":\"https://www.youtube.com/watch?v=QAUnu8AK9V0\"}"])

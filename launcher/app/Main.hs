{-# LANGUAGE DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NamedFieldPuns, PartialTypeSignatures,
             ScopedTypeVariables, TypeApplications #-}

module Main where

import Preludium

import Control.Monad.Except             (ExceptT, MonadError, runExceptT)
import Control.Monad.State.Strict       (StateT)
import Data.Aeson.Lens
import Data.Attoparsec.ByteString       (parseOnly)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Binary.Get                  (getInt32host, runGetOrFail)
import Data.Yaml                        (FromJSON, decodeFile)
import Pipes                            (Producer)
import System.Directory
import System.Process                   (callCommand)
import URI.ByteString

import qualified Pipes
import qualified Pipes.ByteString as PipesBS
import qualified Pipes.Parse      as PipesParse

newtype App m a = App (ReaderT Config (ExceptT AppError Identity) a)
  deriving (Functor, Applicative, Monad
           , MonadError AppError
           , MonadReader Config)

data AppError = EmptyStreamError
              | MessageLengthParsingError Text
              | InvalidMessageError

er :: Text -> Text
er = mappend "Error: "

showAppError :: AppError -> Text
showAppError EmptyStreamError                = er "Empty stream"
showAppError (MessageLengthParsingError msg) = er msg
showAppError InvalidMessageError             = er "Invalid message"

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

quote :: (Semigroup a, IsString a) => a -> a
quote str = "\"" <> str <> "\""

baseOptions :: MonadReader Config m => m [Text]
baseOptions = do
  Config {options, flags} <- ask
  return
    <| map ("--" <>) flags
    <> ifoldMap (\k v -> ["--" <> k <> " " <> quote v]) options

youtubeOptions :: URI -> ReaderT Config Maybe [Text]
youtubeOptions uri = do
  let query = uri ^. queryL . queryPairsL
  list <- lookup "list" query |> lift
  let idx :: Int = (lookup "index" query >>= hush . parseOnly decimal) ?: 1
      url = uri
            |> queryL . queryPairsL .~ [("list", list)]
            |> pathL .~ "/playlist"
            |> serializeURIRef'
  options <- hoist generalize baseOptions
  options
    `snoc` ("--playlist-start " <> toS (show (idx - 1)))
    `snoc` quote (toS url)
    |> return

optionHandlers :: Map ByteString (URI -> ReaderT Config Maybe [Text])
optionHandlers = mapFromList [("www.youtube.com", youtubeOptions)]

specialOptions :: Text -> ReaderT Config Maybe [Text]
specialOptions url = do
  uri <- url |> toS |> parseURI strictURIParserOptions |> hush
  host <- uri ^? authorityL . _Just . authorityHostL . hostBSL |> lift
  options <- optionHandlers ^. at host |> lift
  options uri

data ConfigError = ConfigPlatformError
                 | ConfigFileError FilePath
                 | ConfigParseError

showConfigError :: ConfigError -> Text
showConfigError ConfigPlatformError = "Unsupported platform"
showConfigError (ConfigFileError dir) =
  "Can't find config file: " <> toS (dir </> configFileName)
showConfigError ConfigParseError = "Can't parse config"

appName :: FilePath
appName = "play_in_mpv"

configFileName :: FilePath
configFileName = "play_in_mpv.config.yaml"

readConfig :: ExceptT ConfigError IO Config
readConfig = do
  dir <- getAppUserDataDirectory appName
         |> syncIO
         |> fmapLT (const ConfigPlatformError)
  (dir </> configFileName
   |> decodeFile
   |> syncIO
   |> fmapLT (const <| ConfigFileError dir))
    >>= failWith ConfigParseError

drawBytes :: Monad m
          => Int
          -> ExceptT AppError (StateT (Producer ByteString m x) m) LByteString
drawBytes n = replicateM n (lift PipesBS.drawByte)
              >>= sequence
              >> note EmptyStreamError
              |*> pack

readUrl :: Monad m
        => ExceptT AppError (StateT (Producer ByteString m x) m) Text
readUrl = do
  n <- drawBytes 4
       >>= runGetOrFail getInt32host
       >> map (view _3)
       >> over _Left (MessageLengthParsingError . toS . view _3)
       >> hoistEither
  drawBytes (fromIntegral n)
    |*> preview (key "link" . _String)
    >>= note InvalidMessageError

playCommand :: Text -> Reader Config Text
playCommand url = do
  bo <- snoc <*| baseOptions <*> pure (quote url)
  so <- ask |*> runReaderT (specialOptions url)
  player <- asks player
  cons player (so ?: bo) |> unwords |> return

spawnPlayer :: Text -> ReaderT Config IO ()
spawnPlayer = playCommand >> hoist generalize
              >=> toS >> callCommand >> fork >> void >> lift

runApp :: (Text -> ReaderT Config IO ()) -> Producer ByteString IO () -> IO ()
runApp action stream = runExceptT readConfig
                       >>= either (putStrLn . showConfigError) app
  where
    app config = (action >> (`runReaderT` config) >> lift
                   |> Pipes.for (PipesParse.parsed (runExceptT readUrl) stream)
                   |> Pipes.runEffect)
                 >>= fst >> showAppError >> putStrLn

main :: IO ()
main = runApp spawnPlayer PipesBS.stdin

test :: IO ()
test = runApp printCommand testin
  where
    printCommand = playCommand >> hoist generalize >=> putStrLn
    testin = Pipes.each
      ["6\NUL\NUL\NUL{\"link\":\"https://www.youtube.com/watch?v=QAUnu8AK9V0\"}"
      ,"S\NUL\NUL\NUL{\"link\":\"https://www.youtube.com/playlist?list=PLH-huzMEgGWDi0v0gudmh_2Qt1F9xKjn0\"}"
      ,"g\NUL\NUL\NUL{\"link\":\"https://www.youtube.com/watch?v=ObNgutz0wMQ&list=PLH-huzMEgGWDi0v0gudmh_2Qt1F9xKjn0&index=65\"}"]

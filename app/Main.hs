{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager, Request, parseUrl, setQueryString, httpLbs, responseBody, Manager)
import Web.Authenticate.OAuth (signOAuth, def, oauthConsumerSecret, oauthConsumerKey, Credential(..), oauthServerName, OAuth)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Monoid ((<>))
import Web.Twitter.Types (Status(..))
import Data.Aeson (decode, encode)
import Control.Monad.IO.Class
import Pipes
import qualified Pipes.Prelude as P
import Data.Foldable (traverse_)
import Control.Exception.Base (Exception)
import Safe (lastMay)
import Data.Maybe (catMaybes)
import Control.Monad (forever)
import Options.Applicative (Parser, long, help, option, auto, optional, info, fullDesc, progDesc, ParserInfo, execParser, helper, strOption)

main :: IO ()
main = do
  config <- execParser configParserInfo
  let oauth = twitterOAuth config
  let credential = twitterCredential config
  let limitPipe = maybe identityPipe P.take (configStatusCount config)
  let maxId = BS8.pack <$> (configMaxId config)
  let screenName = BS8.pack $ configScreenName config
  manager <- newManager tlsManagerSettings
  runEffect $ getUserTimeline' screenName maxId oauth credential manager >-> 
              limitPipe >-> 
              statusToJson >-> 
              printByteString

twitterOAuth :: Config -> OAuth
twitterOAuth config = def 
  { oauthServerName = "Twitter"
  , oauthConsumerKey = BS8.pack $ configConsumerKey config
  , oauthConsumerSecret = BS8.pack $ configConsumerSecret config
  }

twitterCredential :: Config -> Credential
twitterCredential config = Credential [(BS8.pack $ configToken config, BS8.pack $ configTokenSecret config)]

userTimelineReq :: MonadThrow m => BS.ByteString -> Maybe BS.ByteString -> m Request
userTimelineReq screenName maxId = do
  initReq <- parseUrl "https://api.twitter.com/1.1/statuses/user_timeline.json" 
  let params = catMaybes [ Just ("screen_name", Just screenName)
                         , fmap (\x -> ("max_id", Just x)) maxId
                         ]
  return $ setQueryString params initReq

intToByteString :: Int -> BS.ByteString
intToByteString = BS8.pack . show

getUserTimeline :: (MonadThrow m, MonadIO m) => BS.ByteString -> OAuth -> Credential -> Manager -> Producer Status m ()
getUserTimeline screenName oauth credential manager = getUserTimeline' screenName Nothing oauth credential manager

getUserTimeline' :: 
  (MonadThrow m, MonadIO m) => 
  BS.ByteString ->
  Maybe BS.ByteString ->
  OAuth ->
  Credential ->
  Manager ->
  Producer Status m ()
getUserTimeline' screenName maxId oauth credential manager = do
    tlReq       <- lift $ userTimelineReq screenName maxId
    signedTlReq <- liftIO $ signOAuth oauth credential tlReq
    response    <- liftIO $ httpLbs signedTlReq manager
    statuses    <- lift $ parseStatuses (responseBody response)
    traverse_ yield statuses
    case getMaxId statuses of
      Just newMaxId ->
        -- First tweet will be a duplicate of the last tweet in the previous request, so it is dropped.
        getUserTimeline' screenName (Just newMaxId) oauth credential manager >-> P.drop 1
      Nothing -> return ()

-- Return the ID of the last tweet in the list if there is one.
getMaxId :: [Status] -> Maybe BS.ByteString
getMaxId statuses = fmap (BS8.pack . show . statusId) (lastMay statuses)

-- Parse into Status objects, throwing a ParseException if the string
-- cannot be parsed.
parseStatuses :: MonadThrow m => BL.ByteString -> m [Status]
parseStatuses str = 
  maybe 
    (throwM (ParseException ("Failed to parse: " <> (BL.unpack str))))
    return
    (decode str :: Maybe [Status])

data ParseException = ParseException String deriving (Show)

instance Exception ParseException

-- Serialize status back into JSON.
statusToJson :: MonadThrow m => Pipe Status BS.ByteString m ()
statusToJson = forever $ await >>= yield . BL.toStrict . encode

printByteString :: MonadIO m => Consumer BS.ByteString m ()
printByteString = forever $ await >>= (liftIO . BS8.putStrLn)

identityPipe :: Monad m => Pipe a a m ()
identityPipe = forever $ await >>= yield

data Config = Config { configConsumerSecret :: String
                     , configConsumerKey :: String
                     , configToken :: String
                     , configTokenSecret :: String
                     , configScreenName :: String
                     , configStatusCount :: Maybe Int
                     , configMaxId :: Maybe String
                     } deriving (Show)

configParserInfo :: ParserInfo Config
configParserInfo = info (helper <*> configParser) (fullDesc <> progDesc "A utility for downloading a user's tweets.")

configParser :: Parser Config
configParser = Config 
  <$> strOption (long "csecret" <> help "Consumer secret.")
  <*> strOption (long "ckey" <> help "Consumer key.")
  <*> strOption (long "atoken" <> help "Access token.")
  <*> strOption (long "asecret" <> help "Access token secret.")
  <*> strOption (long "screenname" <> help "Screen name of account to download from.")
  <*> (optional (option auto 
        (long "count" 
        <> help "Number of tweets to download. If omitted, as many tweets as the API allows will be downloaded."
        )))
  <*> (optional (strOption 
        (long "id"
        <> help "Tweet ID to start with (inclusive). If omitted, download will begin with most recent tweet."
        )))

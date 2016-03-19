{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base     (Exception)
import           Control.Monad              (forever)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Data.Aeson                 (encode, json', Value(..), Array)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable              (traverse_)
import           Data.Monoid                ((<>))
import           Network.HTTP.Client        (Manager, Request, httpLbs,
                                             newManager, parseUrl, responseBody,
                                             setQueryString)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Options.Applicative        (Parser, ParserInfo, auto,
                                             execParser, fullDesc, help, helper,
                                             info, long, option, optional,
                                             progDesc, strOption, value, switch)
import           Pipes
import qualified Pipes.Prelude              as P
import           Safe                       (lastMay)
import           Web.Authenticate.OAuth     (Credential (..), OAuth, def,
                                             oauthConsumerKey,
                                             oauthConsumerSecret,
                                             oauthServerName, signOAuth)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)
import qualified Data.Vector as V 
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import Lens.Micro.Aeson (key)

main :: IO ()
main = do
  config <- execParser configParserInfo
  
  let oauth = twitterOAuth config
  let credential = twitterCredential config
  let maxId = BS8.pack <$> (configMaxId config)
  let screenName = BS8.pack $ configScreenName config
  let statusCount = configStatusCount config
  let params = trimUserParam `unlessMonoid` configTrimUser config
            <> excludeRepliesParam `unlessMonoid` configExcludeReplies config
            <> contributorDetailsParam `unlessMonoid` configContribDetails config
            <> excludeRtsParam `unlessMonoid` configExcludeRts config
  
  manager <- newManager tlsManagerSettings
  runEffect $ getUserTimeline screenName maxId params oauth credential manager >->
              P.take statusCount >->
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

userTimelineReq :: MonadThrow m => BS.ByteString -> QueryParams -> m Request
userTimelineReq screenName params = do
  initReq <- parseUrl "https://api.twitter.com/1.1/statuses/user_timeline.json"
  return $ setQueryString (screenNameParam screenName <> params) initReq

type QueryParams = [(BS.ByteString, Maybe BS.ByteString)]

maxIdParam :: BS.ByteString -> QueryParams
maxIdParam i = [("max_id", Just i)]

screenNameParam :: BS.ByteString -> QueryParams
screenNameParam sn = [("screen_name", Just sn)]

trimUserParam :: QueryParams
trimUserParam = [("trim_user", Just "true")]

excludeRepliesParam :: QueryParams
excludeRepliesParam = [("exclude_replies", Just "true")]

contributorDetailsParam :: QueryParams
contributorDetailsParam = [("contributor_details", Just "true")]

excludeRtsParam :: QueryParams
excludeRtsParam = [("include_rts", Just "false")]

unlessMonoid :: Monoid m => m -> Bool -> m
unlessMonoid _ False = mempty
unlessMonoid m True = m

getUserTimeline ::
  (MonadThrow m, MonadIO m) =>
  BS.ByteString ->
  Maybe BS.ByteString ->
  QueryParams ->
  OAuth ->
  Credential ->
  Manager ->
  Producer Value m ()
getUserTimeline screenName maybeMaxId extraParams oauth credential manager = do
    let params  = (maybe mempty maxIdParam maybeMaxId) <> extraParams
    tlReq       <- lift $ userTimelineReq screenName params
    signedTlReq <- liftIO $ signOAuth oauth credential tlReq
    response    <- liftIO $ httpLbs signedTlReq manager
    statuses    <- lift $ parseStatuses (BL.toStrict (responseBody response))
    traverse_ yield statuses
    case getMaxId statuses of
      Just newMaxId ->
        -- First tweet will be a duplicate of the last tweet in the previous request, so it is dropped.
        getUserTimeline screenName (Just newMaxId) extraParams oauth credential manager >-> P.drop 1
      Nothing -> return ()

-- Return the ID of the last tweet in the list if there is one.
getMaxId :: Array -> Maybe BS.ByteString
getMaxId statuses = undefined
  --do
  --lastStatus <- safeVecLast statuses
  --case lastStatus of
  --  Object hashMap -> do
  --    lastId <- HM.lookup "id_str" hashMap
  --    return $ T.encodeUtf8 lastId
  --  _ -> Nothing

safeVecLast :: Array -> Maybe Value
safeVecLast v = if V.length v > 0 then Just (V.last v) else Nothing 

-- Parse into Status objects, throwing a ParseException if the string
-- cannot be parsed.
parseStatuses :: MonadThrow m => BS.ByteString -> m Array
parseStatuses str = do
  parsed <- case parseOnly (json' <* endOfInput) str of
              Left errStr -> throwM (ParseException ("Parse failed with: " <> errStr <> "\n" <> (BS8.unpack str)))
              Right v -> return v
  case parsed of
    Array ar -> return ar
    _ -> throwM (ParseException ("Expected an array: " <> (BS8.unpack str)))

data ParseException = ParseException String deriving (Show)

instance Exception ParseException

-- Serialize status back into JSON.
statusToJson :: MonadThrow m => Pipe Value BS.ByteString m ()
statusToJson = forever $ await >>= yield . BL.toStrict . encode

printByteString :: MonadIO m => Consumer BS.ByteString m ()
printByteString = forever $ await >>= (liftIO . BS8.putStrLn)

data Config = Config { configConsumerKey    :: String
                     , configConsumerSecret :: String
                     , configToken          :: String
                     , configTokenSecret    :: String
                     , configScreenName     :: String
                     , configStatusCount    :: Int
                     , configMaxId          :: Maybe String
                     , configTrimUser :: Bool
                     , configExcludeRts :: Bool
                     , configContribDetails :: Bool
                     , configExcludeReplies :: Bool
                     } deriving (Show)

configParserInfo :: ParserInfo Config
configParserInfo = info (helper <*> configParser) (fullDesc <> progDesc "A utility for downloading a user's tweets. A tweet siphon.")

configParser :: Parser Config
configParser = Config
  <$> strOption (long "key" <> help "Consumer key.")
  <*> strOption (long "secret" <> help "Consumer secret.")
  <*> strOption (long "token" <> help "Access token.")
  <*> strOption (long "token_secret" <> help "Access token secret.")
  <*> strOption (long "screenname" <> help "Screen name of account to download from.")
  <*> option auto
        (long "count"
        <> help "Number of tweets to download. If omitted, as many tweets as the API allows (3200) will be downloaded."
        <> value 3200
        )
  <*> (optional (strOption
        (long "id"
        <> help "Tweet ID to start with (inclusive). If omitted, download will begin with most recent tweet."
        )))
  <*> switch (long "trim_user" <> help "Include only the author's numerical ID. Trim other details.")
  <*> switch (long "exclude_rts" <> help "Exclude native retweets from results.")
  <*> switch (long "contrib_details" <> help "Include additional contributor info, rather than just the user's ID.")
  <*> switch (long "exclude_replies" <> help "Exclude replies from results.")

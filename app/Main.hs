{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base     (Exception, catch)
import           Control.Monad              (forever)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Data.Aeson                 (Value (..), encode)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable              (find, traverse_)
import           Data.Monoid                ((<>))
import qualified Data.Text.Encoding         as T
import           Lens.Micro                 (toListOf, (^?), _last)
import           Lens.Micro.Aeson           (key, values, _String)
import           Network.HTTP.Client        (HttpException (..), Manager,
                                             Request, Response, httpLbs,
                                             newManager, parseUrl, responseBody,
                                             setQueryString)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Header  (Header, HeaderName)
import           Network.HTTP.Types.Status  (Status (..))
import           Options.Applicative        (Parser, ParserInfo, auto,
                                             execParser, footer, fullDesc, help,
                                             helper, info, long, option,
                                             optional, progDesc, strOption,
                                             switch, value)
import           Pipes
import qualified Pipes.Prelude              as P
import           System.Exit                (ExitCode (..), exitWith)
import           System.IO                  (hPutStrLn, stderr)
import           Web.Authenticate.OAuth     (Credential (..), OAuth, def,
                                             oauthConsumerKey,
                                             oauthConsumerSecret,
                                             oauthServerName, signOAuth)

main :: IO ()
main = do
  config <- execParser configParserInfo

  let oauth = twitterOAuth config
  let credential = twitterCredential config
  let maxId = BS8.pack <$> (configMaxId config)
  let statusCount = configStatusCount config
  identifier <- pickScreennameUserId (configScreenName config) (configUserId config)
  let params = trimUserParam `unlessMonoid` configTrimUser config
            <> excludeRepliesParam `unlessMonoid` configExcludeReplies config
            <> contributorDetailsParam `unlessMonoid` configContribDetails config
            <> excludeRtsParam `unlessMonoid` configExcludeRts config
            <> countParam (configBatchSize config)
            <> identifier

  manager <- newManager tlsManagerSettings
  let pipeline = getUserTimeline maxId params oauth credential manager >->
                 P.take statusCount >->
                 statusToJson >->
                 printByteString
  (runEffect pipeline) `catch` handleExceptions

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

findHeader :: HeaderName -> [Header] -> Maybe BS.ByteString
findHeader headerName headers = fmap snd (find (\(name, _) -> name == headerName) headers)

pickScreennameUserId :: MonadThrow m => Maybe String -> Maybe String -> m QueryParams
pickScreennameUserId (Just screenname) _ = return [("screen_name", Just (BS8.pack screenname))]
pickScreennameUserId _ (Just userId) = return [("user_id", Just (BS8.pack userId))]
pickScreennameUserId _ _ = throwM $ ParamException "Must provide either --screenname or --userid."

data ParamException = ParamException String deriving Show
instance Exception ParamException

handleExceptions :: HttpException -> IO ()
handleExceptions (StatusCodeException (Status 429 _) headers _) = do
  let totalPermits     = findHeader "x-rate-limit-limit" headers
  let remainingPermits = findHeader "x-rate-limit-remaining" headers
  let resetTimeStamp   = findHeader "x-rate-limit-reset" headers
  putStrLnErr
    ( "ERROR: Received 429 Over Rate Limit response."
      <> "\n\tTotal permits: "
      <> (show totalPermits)
      <> "\n\tRemaining permits: "
      <> (show remainingPermits)
      <> "\n\tUnix timestamp (UTC) when permits will be reset: "
      <> (show resetTimeStamp))
  exitWith (ExitFailure 429)
handleExceptions e@(StatusCodeException (Status code _) _ _) = do
  putStrLnErr ("ERROR: Received non-2XX response. Response dump: " <> show e)
  exitWith (ExitFailure code)
handleExceptions e = do
  putStrLnErr ("ERROR: Unexpected exception. Please file a bug report: " <> show e)
  exitWith (ExitFailure 1)

twitterOAuth :: Config -> OAuth
twitterOAuth config = def
  { oauthServerName = "Twitter"
  , oauthConsumerKey = BS8.pack $ configConsumerKey config
  , oauthConsumerSecret = BS8.pack $ configConsumerSecret config
  }

twitterCredential :: Config -> Credential
twitterCredential config = Credential [(BS8.pack $ configToken config, BS8.pack $ configTokenSecret config)]

userTimelineReq :: MonadThrow m => QueryParams -> m Request
userTimelineReq params = do
  initReq <- parseUrl "https://api.twitter.com/1.1/statuses/user_timeline.json"
  return $ setQueryString params initReq

execUserTimelineReq ::
  (MonadThrow m, MonadIO m) =>
  QueryParams ->
  OAuth ->
  Credential ->
  Manager ->
  m (Response BL.ByteString)
execUserTimelineReq params oauth credential manager = do
  tlReq        <- userTimelineReq params
  signedTlReq  <- liftIO $ signOAuth oauth credential tlReq
  liftIO $ httpLbs signedTlReq manager

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

countParam :: Int -> QueryParams
countParam c = [("count", Just (BS8.pack (show c)))]

unlessMonoid :: Monoid m => m -> Bool -> m
unlessMonoid _ False = mempty
unlessMonoid m True = m

getUserTimeline ::
  (MonadThrow m, MonadIO m) =>
  Maybe BS.ByteString ->
  QueryParams ->
  OAuth ->
  Credential ->
  Manager ->
  Producer Value m ()
getUserTimeline maybeMaxId extraParams oauth credential manager = do
    let params   = (maybe mempty maxIdParam maybeMaxId) <> extraParams
    response     <- lift $ execUserTimelineReq params oauth credential manager
    let statuses = parseStatuses (BL.toStrict (responseBody response))

    traverse_ yield statuses

    case nextPage statuses maybeMaxId of
      Just newMaxId ->
        getUserTimeline (Just newMaxId) extraParams oauth credential manager >-> P.drop 1
      Nothing -> return ()

-- Decide if the next page should be retreived. Return `Nothing` if it shouldn't.
-- Return the next maxId if it should.
nextPage :: [Value] -> Maybe BS.ByteString -> Maybe BS.ByteString
nextPage [] _ = Nothing
nextPage statuses lastMaxId =
  if getMaxId statuses == lastMaxId
    then Nothing
    else getMaxId statuses

getMaxId :: [Value] -> Maybe BS.ByteString
getMaxId statuses = fmap T.encodeUtf8 (statuses ^? _last . (key "id_str") . _String)

parseStatuses :: BS.ByteString -> [Value]
parseStatuses = toListOf values

-- Serialize status back into JSON.
statusToJson :: MonadThrow m => Pipe Value BS.ByteString m ()
statusToJson = forever $ await >>= yield . BL.toStrict . encode

printByteString :: MonadIO m => Consumer BS.ByteString m ()
printByteString = forever $ await >>= (liftIO . BS8.putStrLn)

data Config = Config { configConsumerKey    :: String
                     , configConsumerSecret :: String
                     , configToken          :: String
                     , configTokenSecret    :: String
                     , configScreenName     :: Maybe String
                     , configUserId         :: Maybe String
                     , configStatusCount    :: Int
                     , configMaxId          :: Maybe String
                     , configTrimUser       :: Bool
                     , configExcludeRts     :: Bool
                     , configContribDetails :: Bool
                     , configExcludeReplies :: Bool
                     , configBatchSize      :: Int
                     } deriving (Show)

configParserInfo :: ParserInfo Config
configParserInfo =
  info
    (helper <*> configParser)
    (fullDesc
      <> progDesc "A utility for downloading a user's tweets. A tweet siphon."
      <> footer "https://github.com/beala/twiphon")

configParser :: Parser Config
configParser = Config
  <$> strOption (long "key" <> help "Consumer key.")
  <*> strOption (long "secret" <> help "Consumer secret.")
  <*> strOption (long "token" <> help "Access token.")
  <*> strOption (long "token_secret" <> help "Access token secret.")
  <*> optional (strOption (long "screenname" <> help "Screen name of account to download from. Do not use in conjunction with userid."))
  <*> optional (strOption (long "userid" <> help "User ID of account to download from. Do not use in conjunction with screenname."))
  <*> option auto
        (long "count"
        <> help "Number of tweets to download. If omitted, as many tweets as the API allows (3200) will be downloaded."
        <> value 3200
        )
  <*> (optional (strOption
        (long "max_id"
        <> help ("Set an upper bound on the tweet ID (inclusive). Don't fetch tweets newer than this ID. "
                <> "If omitted, download will begin with most recent tweet."
                )
        )))
  <*> switch (long "trim_user" <> help "Include only the author's numerical ID. Trim other details.")
  <*> switch (long "exclude_rts" <> help "Exclude native retweets from results.")
  <*> switch (long "contrib_details" <> help "Include additional contributor info, rather than just the user's ID.")
  <*> switch (long "exclude_replies" <> help "Exclude replies from results.")
  <*> option auto
        (long "batch_size" <> value 200 <> help "Tweets to fetch per request. Must be between 2 and 200 inclusive. Defaults to 200.")

# twiphon

A command line utility for downloading a user's tweets. A tweet siphon.

## Install

This tool is written in Haskell, and compiling from source requires [stack](http://docs.haskellstack.org/en/stable/README/).

```
stack setup
stack install
```

## Usage

Before using the program you must generate a consumer key, a consumer secret, an access token, and an access token secret. To generate these, first register an app with read permission at [https://apps.twitter.com/](https://apps.twitter.com/). Then go to the "Keys and Access Tokens" tab and generate the access tokens. Each of these are passed in on the command line. For example, to scrape my 1000 most recent tweets:

```
  twiphon --secret consumer_secret --key consumer_key \
    --token access_token --token_secret access_token_secret \
    --screenname beala --count 1000
```

The tweets will be written to stdout as newline separated JSON objects.

Additional options:
```
$ twiphon --help
Usage: twiphon --key ARG --secret ARG --token ARG --token_secret ARG
               [--screenname ARG] [--userid ARG] [--count ARG] [--max_id ARG]
               [--trim_user] [--exclude_rts] [--contrib_details]
               [--exclude_replies] [--batch_size ARG]
  A utility for downloading a user's tweets. A tweet siphon.

Available options:
  -h,--help                Show this help text
  --key ARG                Consumer key.
  --secret ARG             Consumer secret.
  --token ARG              Access token.
  --token_secret ARG       Access token secret.
  --screenname ARG         Screen name of account to download from. Do not use
                           in conjunction with userid.
  --userid ARG             User ID of account to download from. Do not use in
                           conjunction with screenname.
  --count ARG              Number of tweets to download. If omitted, as many
                           tweets as the API allows (3200) will be downloaded.
  --max_id ARG             Set an upper bound on the tweet ID (inclusive). Don't
                           fetch tweets newer than this ID. If omitted, download
                           will begin with most recent tweet.
  --trim_user              Include only the author's numerical ID. Trim other
                           details.
  --exclude_rts            Exclude native retweets from results.
  --contrib_details        Include additional contributor info, rather than just
                           the user's ID.
  --exclude_replies        Exclude replies from results.
  --batch_size ARG         Status to fetch per request. Must be between 2 and
                           200 inclusive. Defaults to 200.
```

## Rate Limiting

This program quits when rate limited. If you keep running it after getting rate limited you may find yourself blacklisted. Currently the API only allows a user's 3,200 most recent tweets to be downloaded. This tool uses the [GET statuses/user_timeline](https://dev.twitter.com/rest/reference/get/statuses/user_timeline) endpoint. Rate limiting information here: [https://dev.twitter.com/rest/public/rate-limits](https://dev.twitter.com/rest/public/rate-limits).

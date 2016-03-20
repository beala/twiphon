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

See `twiphon --help` for additional options.

## Rate Limiting

This program quits when rate limited. If you keep running it after getting rate limited you may find yourself blacklisted. Currently the API only allows a user's 3,200 most recent tweets to be downloaded. This tool uses the [GET statuses/user_timeline](https://dev.twitter.com/rest/reference/get/statuses/user_timeline) endpoint. Rate limiting information here: [https://dev.twitter.com/rest/public/rate-limits](https://dev.twitter.com/rest/public/rate-limits).

# twiphon

A command line utility for downloading a user's tweets. A tweet siphon.

## Install

Compiling from source requires [stack](http://docs.haskellstack.org/en/stable/README/).

```
stack setup
stack install
```

## Usage

Before using the program you must generate a consumer key, a consumer secret, an access token, and an access token secret. To generate these, first register an app with read permission at [https://apps.twitter.com/](https://apps.twitter.com/). Then go to the "Keys and Access Tokens" tab and generate the access tokens. Each of these are passed in on the command line. For example, to scrape my 1000 most recent tweets:

```
twiphon --screenname beala --count 1000 --ckey my_consumer_key --csecret my_consumer_secret --atoken my_access_token --asecret my_access_token_secret
```

The tweets will be written to stdout as newline separated JSON objects.

See `twiphon --help` for additional information.

## Rate Limiting

This program fails ungracefully when rate limited (the program is killed and an exception is printed to stderr). If you keep running it after getting rate limited you may find yourself blacklisted. Currently the API only allows a user's 3,200 most recent tweets to be downloaded. This tool uses the [GET statuses/user_timeline](https://dev.twitter.com/rest/reference/get/statuses/user_timeline) endpoint. Rate limiting information here: [https://dev.twitter.com/rest/public/rate-limits](https://dev.twitter.com/rest/public/rate-limits).

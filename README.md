# twiphon

A command line utility for downloading a user's tweets. A tweet siphon.

## Install

Compiling from source requires [stack](http://docs.haskellstack.org/en/stable/README/).

```
stack setup
stack install
```

## Usage

Before using the program you must generate a consumer key, a consumer secret, an access token, and an access token secret. To generate these first register an app with read permission at [https://apps.twitter.com/](https://apps.twitter.com/). Then go the "Keys and Access Tokens" tab and generate the access tokens. Each of these are passed in on the command line. For example, to scrape my first 1000 tweets:

```
twiphon --screenname beala --count 1000 --ckey my_consumer_key --csecret my_consumer_secret --atoken my_access_token --asecret my_access_token_secret
```

The tweets will be written to stdout as newline separated JSON objects.

See `twiphon --help` for additional information.

*Warning:* This program fails ungracefully when rate limited (an exception is throw and the program is killed). Since the API allows around 60,000 tweets to be downloaded every 15 minutes, this is fine for many use cases.
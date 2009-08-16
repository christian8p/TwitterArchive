# Introduction

This script will access your publicly available Twitter stream and create a text
file with all the past tweets in JSON format.

The Twitter API currently limits access to a maximum of 3200 tweets in your
timeline. Hence if you have tweeted more often than that, you will not be able
to retrieve beyond 3200 past tweets.

# Usage

The following instructions assume you have Haskell and Cabal installed on your computer.

* Install `json` from Hackage using `cabal install json`

* Usage info is available by calling `runhaskell twitterarchiver.hs -h`

* Execute the script using `runhaskell twitterarchiver.hs -u username`. For now
  only public Twitter accounts are supported.

* Once the script finishes execution, your twitter archive should be in a file
  called `archive.json`, stored in a simplified JSON format. 

* If you call the function the next time using the same file, it will only query
  the API for the latest tweets since you last backed them up.

# Feedback

I am a Haskell noob, so please bear that in mind when you are reading the
code. Mail me any feedback you have at deepak.jois@gmail.com

# Introduction

This script will access your publicly available Twitter stream and create a text
file with all the past tweets.

The Twitter API currently limits access to a maximum of 3200 tweets in your
timeline. Hence if you have tweeted more often than that, you will not be able
to retrieve beyond 3200 past tweets..

# Usage

This guide assumes you have Haskell and Cabal installed on your computer

* Install `json` from Hackage using `cabal install json`

* Modify the script on top to change the `twitterUser` property to your username

* Execute the script using `runhaskell twitterarchiver.hs`

* Once the script finishes execution, your twitter archive should be in a filed
  called `archive.txt`

# Feedback

I am a Haskell noob, so please bear that in mind when you are reading the
code. Mail me any feedback you have at deepak.jois@gmail.com

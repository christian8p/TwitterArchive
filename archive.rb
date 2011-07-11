#!/usr/bin/env ruby
%w( vyom nimbupani divya ).each { |id| system "hs-twitterarchiver -u #{id} -f #{id}.json" }
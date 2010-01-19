#!/usr/bin/env ruby
%w( vyom nimbupani nimbuin ).each { |id| system "hs-twitterarchiver -u #{id} -f #{id}.json" }
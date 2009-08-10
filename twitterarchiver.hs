import Network.Browser
import Network.HTTP
import Network.URI

import Text.JSON
import Text.JSON.Types
import Text.JSON.String

import Ratio

import Data.List

import qualified System.IO.UTF8 as UTF8

data Tweet = Tweet { tweetText :: String, 
                     tweetCreatedAt :: String , 
                     tweetId :: String }

twitterUser = "vyom"
twitterUrl  = "http://twitter.com/"

main = do
         tweetsJSON <- readTwitterStream
         
         let tweets = map extractTweet tweetsJSON                                   
             tweetsString =  map formatTweet tweets
         
         UTF8.writeFile "archive.txt"  (unlines $ intersperse "\n" tweetsString)

extractTweet :: JSValue -> Tweet
extractTweet tweetJSON = Tweet { tweetText = t, tweetCreatedAt = c, tweetId = i  }
                         where
                           os = case tweetJSON of
                                  (JSObject (JSONObject o)) -> o
                           ex k = case lookup k os of
                                    Just (JSString (JSONString s)) -> s
                                    Just (JSRational False a) -> show (numerator a)
                           [t,c,i]  = map ex ["text", "created_at", "id"]

formatTweet :: Tweet -> String
formatTweet tweet = unlines [ (tweetText tweet), 
                              (tweetCreatedAt tweet),
                              formatUrl (tweetId tweet)]
                         where
                           formatUrl statusId =  concat [twitterUrl,
                                                         twitterUser,
                                                         "/status/", 
                                                         statusId]

readTwitterStream = readTwitterStream' 1 []

readTwitterStream' :: Int -> [JSValue] -> IO [JSValue]
readTwitterStream' page tweets = 
    do 
      let url = concat [twitterUrl,
                        "statuses/user_timeline/",
                        twitterUser,
                        ".json?count=200&page=",
                        (show page)]
      if  (page < 21)
        then
            do
              tweetsJSONString <- (readContentsURL url)
              let tweetsJSON = case runGetJSON readJSArray tweetsJSONString of 
                                 Right (JSArray xs) -> xs
                                 _   -> []
              --print tweetsJSON
              if (not (null tweetsJSON))
                then readTwitterStream' (page + 1) (tweets ++ tweetsJSON)
                else return tweets
                
        else return tweets

readContentsURL :: String -> IO String
readContentsURL u = do
  req <- 
    case parseURI u of
      Nothing -> fail ("ill-formed URL: " ++ u)
      Just ur -> return (defaultGETRequest ur)
    -- don't like doing this, but HTTP is awfully chatty re: cookie handling..
  let nullHandler _ = return ()
  (_u, resp) <- browse $ setOutHandler nullHandler >> request req
  case rspCode resp of
    (2,_,_) -> return (rspBody resp)
    _ -> fail ("Failed reading URL " ++ show u ++ " code: " ++ show (rspCode resp))

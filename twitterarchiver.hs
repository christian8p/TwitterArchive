import Network.Browser
import Network.HTTP
import Network.URI

import Text.JSON
import Text.JSON.Types
import Text.JSON.String

import Ratio

import Data.List

import qualified System.IO.UTF8 as UTF8

data Tweet = Tweet { tweetText :: String, tweetCreatedAt :: String , tweetId :: String }

main = do
         tweetsJSON <- readTwitterStream 1 []
         
         let tweets = map extractTweet tweetsJSON
                                   
             tweetsString =  map (\e ->  (tweetText e) ++ "\n" ++ (tweetCreatedAt e) ++ "\n" ++ (tweetId e)) tweets
         
         UTF8.writeFile "archive.txt"  (unlines $ intersperse "\n" tweetsString)

extractTweet tweetJSON = Tweet { tweetText = t, tweetCreatedAt = c, tweetId = i  }
                         where
                           os = case tweetJSON of
                                  (JSObject (JSONObject o)) -> o
                           ex k = case lookup k os of
                                    Just (JSString (JSONString s)) -> s
                                    Just (JSRational False a) -> show (numerator a)
                           t  = ex "text"
                           c  = ex "created_at"
                           i  = ex "id"
                           
readTwitterStream :: Int -> [JSValue] -> IO [JSValue]
readTwitterStream page tweets = 
    do 
      let url = "http://twitter.com/statuses/user_timeline/deeptijois.json?count=200&page=" ++  (show page)
      case  (page < 21) of
        True -> 
            do
              tweetsJSONString <- (readContentsURL url)
              let tweetsJSON = case runGetJSON readJSArray tweetsJSONString of 
                                 Right (JSArray xs) -> xs
                                 _   -> []
              --print tweetsJSON
              case (not (null tweetsJSON))  of
                True -> readTwitterStream (page + 1) (tweets ++ tweetsJSON)
                False -> return tweets
                
        False -> return tweets

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
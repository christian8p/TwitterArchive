import Network.Browser
import Network.HTTP
import Network.URI

import Text.JSON
import Text.JSON.Types
import Text.JSON.String

import Data.List

import qualified System.IO.UTF8 as UTF8

main = do
         tweets <- readTwitterStream 1 []
         
         let lookupTweetText x = case x of
                                   (JSObject (JSONObject os)) -> 
                                       case lookup "text" os of
                                         Just (JSString (JSONString s)) -> s
                                                                                                     
             lookupTweetDate x = case x of
                                   (JSObject (JSONObject os)) -> 
                                       case lookup "created_at" os of 
                                         Just (JSString (JSONString s)) -> s
                                   
             tweetsString =  map (\e ->  (lookupTweetDate e) ++ "\n" ++ (lookupTweetText e)) tweets
         
         UTF8.writeFile "archive.txt"  (unlines $ intersperse "\n" tweetsString)

readTwitterStream :: Int -> [JSValue] -> IO [JSValue]
readTwitterStream page tweets = 
    do 
      let url = "http://twitter.com/statuses/user_timeline/nimbupani.json?count=200&page=" ++  (show page)
      case  (page < 21) of
        True -> 
            do
              print "Retrieving tweests, page " ++ (show page)
              tweetsJSONString <- (readContentsURL url)
              let tweetsJSON = case runGetJSON readJSArray tweetsJSONString of 
                                 Right (JSArray xs) -> xs
                                 _   -> []
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
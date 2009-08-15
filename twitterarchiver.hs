import Network.Browser
import Network.HTTP
import Network.URI

import Text.JSON
import Text.JSON.Types
import Text.JSON.String
import Text.JSON.Pretty

import Ratio

import Data.List
import Data.Maybe ( fromMaybe )

import System.Console.GetOpt
import System
import Control.Monad
import IO
import List
import Char

import qualified System.IO.UTF8 as UTF8

-- data structure for tweet
data Tweet = Tweet { tweetText :: String, 
                     tweetCreatedAt :: String , 
                     tweetId :: String } deriving Show

instance JSON Tweet where
    showJSON (Tweet tweetText tweetCreatedAt tweetId) = makeObj [
                                                                  ("text", JSString (JSONString tweetText)),
                                                                  ("created_at", JSString (JSONString  tweetCreatedAt)), 
                                                                  ("id", JSRational False 
                                                                                    (fromInteger (read tweetId :: Integer)))
                                                                ]
    readJSON tweet = Error "not yet implemented"

twitterUrl  = "http://twitter.com/"

data Options = Options { optUsername :: String } deriving Show

defaultOptions = Options 
  { optUsername = "vyom" }


options :: [ OptDescr (Options -> IO Options) ]
options = [ Option "h" ["help"]
              (NoArg
                 (\_ -> do
    	            prg <- getProgName
                    hPutStrLn stderr (usageInfo prg options)
                    exitWith ExitSuccess))
            "Show help"
          , Option "u" ["username"]
              (ReqArg
                 (\arg opt -> return opt { optUsername = arg })
                 "vyom")
            "Twitter Username"
          ]

main = do
         args <- getArgs
         -- Parse options, getting a list of option actions
         let (actions, nonOptions, errors) = getOpt RequireOrder options args

         -- Here we thread startOptions through all supplied option actions
         opts <- foldl (>>=) (return defaultOptions) actions
         let Options { optUsername = username } = opts

         tweetsJSON <- readTwitterStream username
         
         let tweets        = map extractTweet tweetsJSON                                   
             tweetsString  =  render $  pp_value  $ showJSON tweets
         UTF8.writeFile "archive.txt"  tweetsString

extractTweet :: JSValue -> Tweet
extractTweet tweetJSON = Tweet { tweetText = t, tweetCreatedAt = c, tweetId = i  }
                         where
                           os = case tweetJSON of
                                  (JSObject (JSONObject o)) -> o
                           ex k = case lookup k os of
                                    Just (JSString (JSONString s)) -> s
                                    Just (JSRational False a) -> show (numerator a)
                           [t,c,i]  = map ex ["text", "created_at", "id"]

readTwitterStream username = readTwitterStream' username 1 []

readTwitterStream' :: String -> Int -> [JSValue] -> IO [JSValue]
readTwitterStream' username page tweets = 
    do 
      let url = concat [twitterUrl,
                        "statuses/user_timeline/",
                        username,
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
                then readTwitterStream' username (page + 1) (tweets ++ tweetsJSON)
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

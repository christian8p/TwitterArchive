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
                     tweetId :: Integer } deriving Show

instance JSON Tweet where
    showJSON (Tweet tweetText tweetCreatedAt tweetId) = makeObj [
                                                                  ("text", JSString (JSONString tweetText)),
                                                                  ("created_at", JSString (JSONString  tweetCreatedAt)), 
                                                                  ("id", JSRational False 
                                                                                    (fromInteger tweetId))
                                                                ]
    readJSON tweet = Error "not yet implemented"

twitterUrl  = "http://twitter.com/"

data Options = Options { optUsername :: String,
                         optFilename :: String 
                       } deriving Show

defaultOptions = Options 
  { optUsername = "vyom",
    optFilename = "archive.json"
  }


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
          , Option "f" ["filename"]
              (ReqArg
                 (\arg opt -> return opt { optFilename = arg })
                 "archive.json")
            "Filename"
          ]

main = do
         args <- getArgs
         -- Parse options, getting a list of option actions
         let (actions, nonOptions, errors) = getOpt RequireOrder options args

         -- Here we thread startOptions through all supplied option actions
         opts <- foldl (>>=) (return defaultOptions) actions
         let Options { optUsername = username,
                       optFilename = filename
                     } = opts

         -- Try reading past tweets
         pastTweetsString <- readContentsArchiveFile filename
         let pastTweets = readJSONTweets pastTweetsString

         -- Read Twitter Stream
         tweetsJSON <- readTwitterStream username pastTweets
         
         let tweets        = map extractTweet tweetsJSON                                   
             tweetsString  =  render $  pp_value  $ showJSON tweets -- Encoding to JSON
         -- Write encoded JSON to file
         UTF8.writeFile filename  tweetsString

extractTweet :: JSValue -> Tweet
extractTweet tweetJSON = Tweet { tweetText = t, tweetCreatedAt = c, tweetId = ((read i) :: Integer )  }
                         where
                           os = case tweetJSON of
                                  (JSObject (JSONObject o)) -> o
                           ex k = case lookup k os of
                                    Just (JSString (JSONString s)) -> s
                                    Just (JSRational False a) -> show (numerator a)
                           [t,c,i]  = map ex ["text", "created_at", "id"]

readJSONTweets :: String -> [JSValue]
readJSONTweets tweetsJSONString = case runGetJSON readJSArray tweetsJSONString of
                              Right (JSArray xs) -> xs
                              _                  -> []

readTwitterStream username pastTweets = do
  if (not (null pastTweets))
     then
         do
           let sinceid = maximum (map (tweetId . extractTweet)  pastTweets)           
           putStrLn (show sinceid)
           latestTweets <- readTwitterStream' username 1 [] (Just sinceid)
           return (latestTweets ++ pastTweets)
     else 
           readTwitterStream' username 1 [] Nothing 

readTwitterStream' :: String -> Int -> [JSValue] -> Maybe Integer -> IO [JSValue]
readTwitterStream' username page tweets sinceid = 
    do 
      let url         = twitterUrl ++ "statuses/user_timeline/" ++ username ++ ".json"
          querystring = case sinceid of 
                          Nothing -> "count=200&page=" ++ (show page)
                          Just tweetid ->  "count=200&page=" ++ (show page) ++ "&since_id=" ++ (show tweetid)
          fullUrl = url ++ "?" ++ querystring                   
      if  (page < 21)
        then
            do
              tweetsJSONString <- (readContentsURL fullUrl)
              let tweetsJSON = readJSONTweets tweetsJSONString
              --print tweetsJSON
              if (not (null tweetsJSON))
                then readTwitterStream' username (page + 1) (tweets ++ tweetsJSON) sinceid
                else return tweets
                
        else return tweets

readContentsArchiveFile :: String -> IO String
readContentsArchiveFile f = do
  result <- try (readFile f)
  case result of
    Right s -> do
                putStrLn "Reading archive file"
                return s
                       
    _       -> do
                putStrLn "Could not read archive file"
                return ""

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

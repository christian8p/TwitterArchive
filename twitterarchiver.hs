import Network.Browser
import Network.HTTP
import Network.URI

import Text.JSON
import Text.JSON.Types
import Text.JSON.String
import Text.JSON.Pretty

import Ratio
import Data.List
import System.Console.GetOpt
import System
import List
import Char

import IO
import Control.Monad
import Control.Applicative
import Control.Monad.Reader

import qualified System.IO.UTF8 as UTF8

-- Data structure for tweet
data Tweet = Tweet { tweetText :: String, 
                     tweetCreatedAt :: String , 
                     tweetId :: Integer } deriving Show

data Options = Options { optUsername :: String,
                         optFilename :: String,
                         optPassword :: Maybe String
                      } deriving Show

defaultOptions = Options 
 { optUsername = "vyom",
   optFilename = "archive.json",
   optPassword = Nothing
 }

data TwitterSettings = TS {
   twitterUsername :: String,
   sinceId         :: Maybe Integer    
}

-- Making Tweet typeclass of JSON to enable decode/encode
instance JSON Tweet where
   showJSON (Tweet tweetText tweetCreatedAt tweetId) = makeObj [ ("text", showJSON $ tweetText),
                                                                 ("created_at", showJSON $ tweetCreatedAt), 
                                                                 ("id", showJSON $ tweetId)]

   readJSON (JSObject obj) = let
                       jsonObjAssoc = fromJSObject obj
                   in do
                     i <- mLookup "id"   jsonObjAssoc >>= readJSON
                     t <- mLookup "text" jsonObjAssoc >>= readJSON
                     c <- mLookup "created_at" jsonObjAssoc >>= readJSON
                     return $ Tweet 
                            { tweetText      = t,
                              tweetCreatedAt = c,
                              tweetId        = i
                            }

-- Misc          
mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)
twitterUrl  = "http://twitter.com/"

-- Options Handling
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
          , Option "p" ["password"]
              (ReqArg
                 (\arg opt -> return opt { optPassword = Just arg })
                 "password")
            "Password"
            
          ]

calculateSinceId pastTweets = if (not (null pastTweets))
                                then
                                  let (Ok tweetids) = forM pastTweets $ (liftM tweetId) . readJSON
                                  in Just (maximum tweetids)
                                else Nothing

readJSONTweets tweetsJSONString = case runGetJSON readJSArray tweetsJSONString of
                             Right (JSArray xs) -> xs
                             _                  -> []

readTwitterStream = readTwitterStream' 1 [] 

readTwitterStream' page tweets
                        | page >= 21 = return tweets
                        | otherwise  = do
                                          sinceId  <- asks sinceId
                                          username <- asks twitterUsername                          
                                          tweetsJSONString <- liftIO $ readContentsURL (fullUrl username sinceId)
                                          let tweetsJSON = readJSONTweets tweetsJSONString
                                          if (not (null tweetsJSON))
                                            then readTwitterStream' (page + 1) (tweets ++ tweetsJSON)
                                            else return tweets

                                       where url username          = twitterUrl ++ "statuses/user_timeline/" ++ username ++ ".json"
                                             queryParams           = [("count", "200"), ("page", show page)]
                                             concatQueryStr params = intercalate "&" $ map (\(k,v) -> k ++ "=" ++ v) params    
                                             -- Add since_id to params if value exists
                                             querystring sinceId   = case sinceId of 
                                                                       Nothing -> concatQueryStr queryParams
                                                                       Just tweetId ->  concatQueryStr $ queryParams ++ [("since_id", show tweetId)]
                                             fullUrl username sinceId = (url username) ++ "?" ++ (querystring sinceId)

readContentsArchiveFile f = do
 result <- try (readFile f)
 case result of
   Right s -> do
               putStrLn "Reading archive file"
               return s

   _       -> do
               putStrLn "Could not read archive file"
               return ""

readContentsURL u = do
 putStrLn u
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
         pastTweets <- readJSONTweets <$> (readContentsArchiveFile filename)
         
         let settings = TS { twitterUsername = username,
                             sinceId         = calculateSinceId pastTweets
                           }
         latestTweets <- runReaderT readTwitterStream settings
         let allTweetsJSON   = latestTweets ++ pastTweets
             (Ok tweets)     = mapM readJSON allTweetsJSON :: Result [Tweet]                                    
             tweetsString    =  render $  pp_value  $ showJSON tweets -- Encoding to JSON
         
         -- Write encoded JSON to file
         UTF8.writeFile filename  tweetsString
         
         
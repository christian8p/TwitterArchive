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

import Data.Maybe

import System.IO
import IO
import Control.Monad
import Control.Applicative
import Control.Monad.Reader

import qualified System.IO.UTF8 as UTF8

-- Data
data Tweet = Tweet { 
  tweetText      :: String, 
  tweetCreatedAt :: String , 
  tweetId        :: Integer } deriving Show

data Options = Options { 
  optUsername :: String,
  optFilename :: String,
  optPassword :: Maybe String } deriving Show

data TwitterSettings = TS {
   tsUsername :: String,
   tsSinceId  :: Maybe Integer,
   tsPassword :: Maybe String
}

-- Making Tweet typeclass of JSON to enable decode/encode
instance JSON Tweet where
   showJSON (Tweet tweetText tweetCreatedAt tweetId) = makeObj [ ("text", showJSON tweetText),
                                                                 ("created_at", showJSON tweetCreatedAt), 
                                                                 ("id", showJSON tweetId)]

   readJSON (JSObject obj) = let
                       jsonObjAssoc = fromJSObject obj
                   in do
                     i <- mLookup "id"   jsonObjAssoc >>= readJSON
                     t <- mLookup "text" jsonObjAssoc >>= readJSON
                     c <- mLookup "created_at" jsonObjAssoc >>= readJSON
                     return Tweet 
                            { tweetText      = t,
                              tweetCreatedAt = c,
                              tweetId        = i
                            }

-- Misc          
mLookup a = maybe (fail $ "No such element: " ++ a) return . lookup a
twitterUrl  = "http://twitter.com/"

-- Options Handling
defaultOptions = Options { 
   optUsername = "vyom",
   optFilename = "archive.json",
   optPassword = Nothing }

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
              (NoArg
                 (\opt -> do
                            putStr "Enter Twitter Password : "
                            hFlush stdout
                            hSetEcho stdout False                            
                            password <- hGetLine stdin
                            hSetEcho stdout True
                            putStr "\n"
                            return opt { optPassword = Just password }))
            "ask for password (private Twitter stream)"            
          ]

-- calculate latest id for since_id param
calculateSinceId pastTweets = if not (null pastTweets)
                                then
                                  let (Ok tweetids) = forM pastTweets $ liftM tweetId . readJSON
                                  in Just (maximum tweetids)
                                else Nothing

-- extract array of JSON values from a string
readJSONTweets tweetsJSONString = case runGetJSON readJSArray tweetsJSONString of
                             Right (JSArray xs) -> xs
                             _                  -> []

-- read twitter stream
readTwitterStream = readTwitterStream' 1 [] 

-- read twitter stream page by page
readTwitterStream' page tweets
    | page >= 21 = return tweets
    | otherwise  = do
                      sinceId  <- asks tsSinceId
                      username <- asks tsUsername                          
                      tweetsJSON <- readJSONTweets <$> readContentsURL (fullUrl username sinceId)
                      if not (null tweetsJSON)
                        then readTwitterStream' (page + 1) (tweets ++ tweetsJSON)
                        else return tweets
                   where url username          = twitterUrl ++ "statuses/user_timeline/" ++ username ++ ".json"
                         queryParams           = [("count", "200"), ("page", show page)]
                         concatQueryStr params = intercalate "&" $ map (\(k,v) -> k ++ "=" ++ v) params
                         querystring Nothing        =  concatQueryStr queryParams
                         querystring (Just tweetId) =  concatQueryStr $ queryParams ++ [("since_id", show tweetId)]
                         fullUrl username sinceId = url username ++ "?" ++ querystring sinceId

-- read JSON contents of an archive file on disk
readContentsArchiveFile f = do
 result <- try (readFile f)
 case result of
   Right s -> do
               putStrLn "Reading archive file"
               return s

   _       -> do
               putStrLn "Could not read archive file"
               return ""


-- read contents of URL w/ optional HTTP auth
readContentsURL u = do
 liftIO $ putStrLn u
 
 username <- asks tsUsername
 password <- asks tsPassword 
 
 -- don't like doing this, but HTTP is awfully chatty re: cookie handling..
 let nullHandler _ = return ()
 (_u, resp) <- liftIO $ browse $ do
      setOutHandler nullHandler
      checkAuth username password
      (request $ getRequest u)
 
 case rspCode resp of
   (2,_,_) -> return (rspBody resp)
   _ -> fail ("Failed reading URL " ++ show u ++ " code: " ++ show (rspCode resp))

-- checks if password is provided and sets HTTP auth for request
checkAuth username password 
   | password == Nothing = return ()
   | otherwise = do 
        ioAction $ putStrLn "Using HTTP Auth"
        addAuthority auth -- add auth to request
         where auth = AuthBasic {
                auUsername = username,
                auPassword = fromJust password,
                auRealm    = "",
                auSite      = fromJust $ parseAbsoluteURI twitterUrl }
         
main = do
         args <- getArgs
         -- Parse options, getting a list of option actions
         let (actions, nonOptions, errors) = getOpt RequireOrder options args

         -- Here we thread startOptions through all supplied option actions
         opts <- foldl (>>=) (return defaultOptions) actions
         let Options { optUsername = username,
                       optFilename = filename,
                       optPassword = password
                     } = opts                     
         
         -- Read past tweets (if any)
         pastTweets <- readJSONTweets <$> readContentsArchiveFile filename
         
         -- init settings
         let settings = TS { tsUsername = username,
                             tsSinceId  = calculateSinceId pastTweets,
                             tsPassword = password }
                             
         -- fetch latest tweets                     
         latestTweets <- runReaderT readTwitterStream settings
         
         -- format tweets into string
         let allTweetsJSON   = latestTweets ++ pastTweets -- combine past and latest tweets
             (Ok tweets)     = mapM readJSON allTweetsJSON :: Result [Tweet]                                    
             tweetsString    =  render $  pp_value  $ showJSON tweets -- Encoding to JSON
         
         -- Write encoded JSON to file
         UTF8.writeFile filename  tweetsString
         
         
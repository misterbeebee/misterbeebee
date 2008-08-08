--
-- A simple, clean IRC bot in Haskell
--
--  $ ghc -O --make -o bot bot.hs
--  $ ./bot
-- or
--  $ runhaskell bot.hs
-- or
--  $ echo main | ghci bot.hs
-- or
--  $ echo main | hugs -98 bot.hs
-- or
--  $ runhugs -98 bot.hs
--
 
import Data.List
import Network
import System.Environment
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch)


-- SVN Stuff
import Text.ChangeMonger.Subversion
-- End SVN Stuff

server = "chat.freenode.net"
port   = 6667
chan   = "#bigriver-icfp08"
nick   = "haskellbot"
 
-- SVN Stuff
svnURL =  "svn+ssh://icfp@svn.openomy.com/u/icfp/repo"  

getSvnChanges :: IO String
getSvnChanges = svnChanges svnURL

separator :: String
separator = (take 72 (repeat '-'))

lastChange :: String -> String
lastChange = (unlines) . fst .  break (== separator) . (drop 1) . lines

getSvnLatest :: IO String
getSvnLatest  = liftM lastChange getSvnChanges

-- End SVN Stuff
--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
 
--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: [String] -> IO ()
mainWithArgs args =  bracket connect disconnect loop
    where
      disconnect = hClose . socket
      loop st    = catch (runReaderT behavior st) (const $ return ())
      behavior = 
        case args of
          ["post-commit"] -> (do; svnLatest <- io getSvnLatest; run $ oneOff svnLatest)
          [] -> run listen


--
-- Connect to the server and return the initial bot state
--
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
 
--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
-- 
run :: (Handle -> Net ()) -> Net ()
run behavior = do
    joinChannel
    asks socket >>= behavior

joinChannel :: Net ()
joinChannel = do
    write "NICK" nick
    write "USER" (nick++" 0 * : bot")
    write "JOIN" chan

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- read initial info from server, then send my message, and log off.
oneOff :: String -> Handle -> Net ()
oneOff msg h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if serverBecameReady s then (privmsg msg >> write "QUIT" ":Exiting" >> io (exitWith ExitSuccess))  else return ()
  where
   serverBecameReady s = "End of /NAMES list" `isInfixOf` s

--
-- Dispatch a command
--
eval :: String -> Net ()
eval     "!svnlatest"          = (do; svnLatest <- io getSvnLatest; privmsg svnLatest)
eval     "!uptime"             = uptime >>= privmsg
eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval     _                     = return () -- ignore everything else
 
--
-- Send a privmsg to the current chan + server
-- mrbb: 2008-07: Send one message per line, and NOTICE is safer than PRIVMSG
--
privmsg :: String -> Net ()
privmsg = sequence_ . map (\s -> write "NOTICE" (chan ++ " :" ++ s)) . lines
 
--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
 
--
-- Calculate and pretty print the uptime
--
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero
 
--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s
 
--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO

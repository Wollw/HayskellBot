import Data.List
import Data.List.Split
import Data.Map
import Network
import System.IO
import System.Exit
import System.Posix
import Control.Arrow
import Control.Monad.Reader
import Control.OldException
import Text.Printf
import Prelude hiding (catch)

import Message

botCommands = fromList
    [ ( "quit", write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)    )
    --, ( "echo", privmsg (drop 5 x)                                      )
    , ( "fall", privmsg "\1ACTION falls over.\1"                        ) ]

server = "irc.canternet.org"
port   = 6667
chan   = "#HackingIsMagic"
nick   = "HayskellBot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

-- Setup actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (const $ return ())

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s ..." server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :hayskell bot")
    asks socket >>= listen

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    let m = parseMessage s
    if ping s then pong s
    else if isConnected $ command m then joinChannel    -- Wait for MOTD before joining
    else if shouldEval (clean s) then eval $ drop 1 $ dropWhile (/= ' ') (clean s)
    else return ()
  where
    forever a   = a >> forever a
    clean       = drop 1 . dropWhile(/= ':') . drop 1
    ping x      = "PING :" `isPrefixOf` x
    pong x      = write "PONG" (':' : drop 6 x)
    isConnected = (==) $ CommandNum 1
    joinChannel = write "JOIN" chan
    shouldEval  = isPrefixOf ("!"++nick++" ")

-- Dispatch a command
eval :: String -> Net ()
eval s =
    case Data.Map.member s botCommands of
        True  -> case Data.Map.lookup s botCommands of
                    Just x  -> x
                    Nothing -> return ()
        False -> return ()

-- Send a privmsg to the server we're currently connected to
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t


-- Convenience.
io :: IO a -> Net a
io = liftIO

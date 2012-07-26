module IRCBot where
import Network
import System.IO
import Data.List

import IRCBot.Message

import Control.Exception as C
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Text.Printf


data IRCBot = IRCBot {
    socket   :: Handle,
    nickname :: String,
    channels :: [String]
}

type IRCServer  = String
type IRCPort    = Int
type IRCNick    = String
type IRCChannel = String

-- Launch a new instance of the bot
launch :: IRCServer -> IRCPort -> IRCNick -> [IRCChannel] -> IO ()
launch s p n cl = bracket (connect s p n cl) disconnect run

run :: IRCBot -> IO ()
run bot = do
    write (socket bot) "NICK" $ nickname bot
    write (socket bot) "USER" $ (nickname bot) ++ " 0 * :" ++ (nickname bot)
    listen bot

listen :: IRCBot -> IO ()
listen bot = forever $ do
    s <- init `fmap` liftIO (hGetLine (socket bot))
    liftIO (putStrLn s)
    let m = parseMessage s
    if ping s then pong s
    else if isConnected $ command m then joinChannel
    else return ()
  where
    forever a = a >> forever a
    clean       = drop 1 . dropWhile(/= ':') . drop 1
    ping x      = "PING :" `isPrefixOf` x
    pong x      = write (socket bot) "PONG" (':' : drop 6 x)
    isConnected = (==) $ CommandNum 1
    joinChannel = write (socket bot) "JOIN" $ ((channels bot) !! 0)

-- Connect to the IRC network
connect :: IRCServer -> IRCPort -> IRCNick -> [IRCChannel] -> IO IRCBot
connect s p n cl = notify $ do
    socket <- connectTo s (PortNumber (fromIntegral p))
    hSetBuffering socket NoBuffering
    return (IRCBot socket n cl)
  where
    notify = bracket_
        (printf "Connecting to %s port %d ..." s p >> hFlush stdout)
        (putStrLn "done.")

-- Close the connect to the IRC network
disconnect :: IRCBot -> IO ()
disconnect b = notify $ do
    (hClose . socket) b
  where
    notify = bracket_
        (printf "Disconnecting..." >> hFlush stdout)
        (putStrLn "done.")

-- Send a message out to the server we're currently connected to
write :: Handle -> String -> String -> IO ()
write h s t = do
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t


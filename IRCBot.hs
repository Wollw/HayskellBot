module IRCBot where

import Network
import System.IO
import System.Exit
import Data.List
import Data.Map
import Control.Exception as C
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Text.Printf

import IRCBot.Message


-- Commands that bot will recognize. Type for lambdas is
--      IRCBot -> Message -> String -> IO ()
botCommands = fromList
    [ ( "quit", \b _ _ -> write b "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess) )
    , ( "explode", \b m _ -> do
                privmsg b $ "\1ACTION explodes leaving mangled bits all over "++(drop 1 $ takeWhile (/= '!') $ prefix m)++".\1"
                write b "QUIT" ":Exploded" >> liftIO (exitWith ExitSuccess) )
    , ( "echo", \b _ s -> privmsg b $ drop 5 s )
    , ( "hug",  \b m s -> case length (drop 4 s) == 0 of
                  True  -> privmsg b $ "\1ACTION hugs "++(drop 1 $ takeWhile (/= '!') $ prefix m)++".\1"
                  False -> privmsg b $ "\1ACTION hugs "++(drop 4 s)++".\1" )
    , ( "fall", \b _ _ -> privmsg b "\1ACTION falls over.\1" ) ]

data IRCBot = IRCBot {
    socket   :: IRCHandle,
    nickname :: IRCNick,
    channels :: [IRCChannel]
}

type IRCServer  = String
type IRCPort    = Int
type IRCNick    = String
type IRCChannel = String
type IRCHandle  = Handle

-- Launch a new instance of the bot
launch :: IRCServer -> IRCPort -> IRCNick -> [IRCChannel] -> IO ()
launch s p n cl = bracket (connect s p n cl) disconnect run

run :: IRCBot -> IO ()
run bot = do
    write bot "NICK" $ nickname bot
    write bot "USER" $ (nickname bot) ++ " 0 * :" ++ (nickname bot)
    listen bot

listen :: IRCBot -> IO ()
listen bot = forever $ do
    s <- init `fmap` liftIO (hGetLine (socket bot))
    liftIO (putStrLn s)
    let m = parseMessage s
    liftIO (printf "length: %d\n" $ length $ params m)
    if length (params m) >= 2 then
        liftIO (putStrLn $ (params m) !! 1)
    else return ()
    
    if ping s then pong s
    else if isConnected $ command m then joinChannels -- Join after connect
    else if length (params m) >= 2 && shouldEval m then do
        liftIO (putStrLn "Eval")
        eval bot m -- Evaluate 
    else do
        return ()
  where
    forever a = a >> forever a
    clean       = drop 1 . dropWhile(/= ':') . drop 1
    ping x      = "PING :" `isPrefixOf` x
    pong x      = write bot "PONG" (':' : drop 6 x)
    isConnected = (==) $ CommandNum 1
    joinChannels  = mapM_ (write bot "JOIN") $ channels bot
    shouldEval m = ((nickname bot)++": ") `isPrefixOf` (params m !! 1)

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
write :: IRCBot -> String -> String -> IO ()
write bot s t = do
    liftIO $ hPrintf (socket bot) "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t


-- Dispatch a command
eval :: IRCBot -> Message -> IO ()
eval bot m = do
    let cmd = drop 1 $ dropWhile (/= ' ') $ params m !! 1
    liftIO (putStrLn $ params m !! 1)
    case Data.Map.lookup (takeWhile (/= ' ') cmd) botCommands of
        Just x  -> x bot m $ cmd
        Nothing -> return ()

-- Send a privmsg to the server we're currently connected to
privmsg :: IRCBot -> String -> IO ()
privmsg bot s = write bot "PRIVMSG" $ ((channels bot) !! 0) ++ " :" ++ s

module IRCBot.Message where
import Safe

data Message =
    MessagePrefixed {
        prefix  :: String,
        command :: Command,
        params  :: Parameters
    }
  | MessageUnprefixed {
        command :: Command,
        params  :: Parameters
    }
    deriving (Show)

data Command = 
    CommandNum Integer
  | CommandStr String
    deriving (Show, Eq)

type Parameters = [String]

parseMessage :: String -> Message
parseMessage str =
    case str of
      (':':xs) -> MessagePrefixed
                    (words str !! 0)
                    (readCommand (words str !! 1))
                    [unwords $ drop 2 (words str)]
      str      -> MessageUnprefixed
                    (readCommand (words str !! 0))
                    [unwords $ drop 1 (words str)]

readCommand :: String -> Command
readCommand str =
    case readMay str of
      Nothing -> CommandStr str
      Just n  -> CommandNum n

messageCommand :: Message -> Command
messageCommand (MessagePrefixed _ c _) = c
messageCommand (MessageUnprefixed c _) = c

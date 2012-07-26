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
                    ((words $ takeWhile (/= ':') (paramString str 2)) ++ [drop 1 $ dropWhile (/= ':') (paramString str 2)])
      str      -> MessageUnprefixed
                    (readCommand (words str !! 0))
                    ((words $ takeWhile (/= ':') (paramString str 1)) ++ [drop 1 $ dropWhile (/= ':') (paramString str 1)])
  where
    paramString s n = (unwords $ drop n (words str))

readCommand :: String -> Command
readCommand str =
    case readMay str of
      Nothing -> CommandStr str
      Just n  -> CommandNum n

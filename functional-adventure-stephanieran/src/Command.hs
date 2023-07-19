module Command where

import Direction
import Item
import Text.Parsec hiding
  ( choice,
    many,
    many1,
    parse,
    sepBy,
    sepBy1,
    (<|>),
  )
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  | Drink
  deriving (Eq, Show)

type Conjunction = [Command]

-- inventoryP only accepts the string 'inventory' and rejects everything else
inventoryP :: Parser Command
inventoryP = pure Inventory <* string "inventory"

-- way to write above, monadic style
inventoryP_monad :: Parser Command
inventoryP_monad = do
    _ <- string "inventory"
    pure Inventory

-- exitP is a parser for the command to quit the game. This parser will accept
-- either the single word 'quit' or the single word 'exit'.
exitP :: Parser Command
exitP = do
    _ <- string "exit" <|> string "quit"
    pure Exit

exitP' :: Parser Command
exitP' =
    pure Exit <* (string "exit" <|> string "quit")

-- parse any string representing an ItemName into an inhabitant of the ItemName datatype
-- doesn't require EOF
itemNameP :: Parser ItemName
itemNameP =
  choice
    [ string "bucket" >> return Bucket,
      string "noodle" >> return Noodle,
      string "towel" >> return Towel,
      string "lock" >> return Lock,
      string "dumbbell" >> return Dumbbell,
      string "bench" >> return Bench,
      string "mat" >> return Mat,
      string "gatorade" >> return Gatorade
    ]

-- nounPhrase_stub is a parser that takes an alphabetic string off the front of
-- the input, then puts it into a singleton list
nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = do
    noun <- itemNameP
    return [noun]

nounPhrase :: Parser [ItemName]
nounPhrase = do
    allNouns <- sepBy itemNameP (char ',' >> optional (char ' '))
    return allNouns

-- takeP parses the word 'take' plus a noun phrase into a Command.
-- There needs to be exactly one space between 'take' and the first word of the noun phrase.
takeP :: Parser Command
takeP = Take <$> (string "take " *> nounPhrase)

dropP :: Parser Command
dropP = Drop <$> (string "drop " *> nounPhrase)

-- only accepts the string "look"
lookP :: Parser Command
lookP = pure Look <* string "look"

-- directionP expects a single lowercase word denoting a direction, making the obvious map
-- from the word 'north', 'south', 'east', or 'west' to the relevant Direction.
directionP :: Parser Direction
directionP =
    choice
        [ string "north" >> return N,
        string "south" >> return S,
        string "east" >> return E,
        string "west" >> return W
        ]

-- moveP parses a move command. It expects one of the four words, 'north', 'south', 'east', or 'west'.
-- It consumes the relevant word off the input, and makes that word into a command telling the game
-- to move in the relevant direction:
moveP :: Parser Command
moveP = Move <$> directionP

drinkP :: Parser Command
drinkP = pure Drink <* string "drink"

-- commandP accepts any single command that is syntactically well-formed, according to the grammar of
-- the game's language, and returns the Command corresponding to the string in the language.
commandP :: Parser Command
commandP = choice [takeP, dropP, moveP, lookP, inventoryP, exitP, drinkP]

-- help with this one
conjunctionP :: Parser Conjunction
conjunctionP = (sepBy1 commandP (string " and ")) <* eof

parseInput :: String -> Maybe Conjunction
parseInput str =
    case parse conjunctionP str of
        Left _ -> Nothing
        Right cmds -> Just cmds
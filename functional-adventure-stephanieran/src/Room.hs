module Room where

import Data.List ()
import Direction
import Item

-- type, data, and variable declarations
data RoomName
  = Lobby
  | StrengthRoom
  | CardioRoom
  | Pool
  | Sauna
  deriving (Eq, Ord)

instance Show RoomName where
  show name = case name of
    Lobby -> "Lobby"
    StrengthRoom -> "Strength Room"
    CardioRoom -> "Cardio Room"
    Pool -> "Pool"
    Sauna -> "Sauna"

type Exit = (Direction, RoomName)

data Room = Room
  { rname :: RoomName,
    desc :: String,
    exits :: [Exit],
    objects :: [ItemName]
  }
  deriving (Show)

hasObjects :: Room -> Bool
hasObjects rm = not $ null (objects rm) -- null returns True if EMPTY, which means it does not have objects, so haveObjects should be False

lobby :: Room
lobby = Room {rname = Lobby, desc = "You are in the Lobby.", exits = [(N, Pool), (E, StrengthRoom), (S, CardioRoom)], objects = [iname lock, iname gatorade]}

sauna :: Room
sauna = Room {rname = Sauna, desc = "You are in the Sauna.", exits = [(S, Pool)], objects = [iname bucket]}

pool :: Room
pool = Room {rname = Pool, desc = "You are at the Pool.", exits = [(S, Lobby), (N, Sauna)], objects = [iname noodle, iname towel]}

strengthRoom :: Room
strengthRoom = Room {rname = StrengthRoom, desc = "You are in the Strength Training Room.", exits = [(W, Lobby)], objects = [iname dumbbell, iname bench]}

cardioRoom :: Room
cardioRoom = Room {rname = CardioRoom, desc = "You are in the Cardio Room.", exits = [(N, Lobby)], objects = [iname mat]}

roomNames :: [RoomName]
roomNames = map rname allRooms

allRooms :: [Room]
allRooms = [lobby, sauna, pool, strengthRoom, cardioRoom]

-- Function Definitions

-- addItem takes an ItemName and a Room as input, and returns a new Room with that
-- ItemName added to its objects record field
addItem :: ItemName -> Room -> Room
addItem itemName rm = rm {objects = itemName : objects rm}

-- removeItem takes an ItemName and a Room as input, and returns a new Room with that
-- ItemName removed from its objects record field
removeItem :: ItemName -> Room -> Room
removeItem itemName rm = rm {objects = filter (/= itemName) (objects rm)}

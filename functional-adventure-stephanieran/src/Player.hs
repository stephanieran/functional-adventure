module Player where

import Data.List ()
import Item
import Room

data Player = Player
  { inventory :: [ItemName],
    maxWeight :: Integer,
    location :: RoomName
  }
  deriving (Show, Eq)

addItem :: ItemName -> Player -> Player
addItem item player = player {inventory = item : inventory player}

-- this assumes no duplicate items in player inventory, assumed in variant
-- maybe use delete instead of filter
removeItem :: ItemName -> Player -> Player
removeItem item player = player {inventory = filter (/= item) (inventory player)}

newLocation :: RoomName -> Player -> Player
newLocation room player = player {location = room}

isCarryingAnything :: Player -> Bool
isCarryingAnything player = not $ null (inventory player)

you :: Player
you = Player {inventory = [], maxWeight = 40, location = Lobby}
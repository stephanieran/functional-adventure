module GameState where

-- modules from base
import Control.Exception

-- modules we wrote
import Data.Map.Strict qualified as M
import Direction
import Item
import Player
  ( Player (inventory, location, maxWeight),
    addItem,
    removeItem,
    you,
  )
import Room
  ( Room (exits, objects, rname),
    RoomName (..),
    addItem,
    allRooms,
    removeItem,
  )

-- DECLARATION OF TYPES, DATA, AND VARIABLS --
type GameMap = M.Map RoomName Room

-- instead of Either String Player or Either String GameState, we'll be able to write Error Player or Error GameState
type Error a = Either String a

data GameState = GameState
  { message :: Maybe String,
    gmap :: GameMap,
    universe :: Universe,
    player :: Player,
    slipperyItems :: [(ItemName, Integer)]
  }
  deriving (Show)

gameMap :: GameMap
gameMap = mkMap allRooms

initialState :: GameState
initialState = GameState {message = Nothing, gmap = gameMap, universe = univ, player = you, slipperyItems = [(Noodle, 3), (Bucket, 2)]}

roomHasObjects :: GameState -> Bool
roomHasObjects st = not $ null (objects $ currentRoom st)

haveWonGame :: GameState -> Bool
haveWonGame st = ((location (player st)) == StrengthRoom) && (elem Mat (currentInventory st))

-- FUNCTION DECLARATIONS --
-- Turns a list of Room-s in a GameMap
mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList $ map (\room -> (rname room, room)) rooms

-- getObject helper function
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u =
    case M.lookup iname u of
        Just obj -> obj
        Nothing -> throw KeyError

-- Looks up Item based on ItemName
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

-- GetRoom helper function
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp =
    case M.lookup rname mp of
        Just room -> room
        Nothing -> throw KeyError

-- Looks up Room based on RoomName
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- Takes a RoomName, a Room, and a GameMap then looks up the room in the input GameMap based
-- on the input RoomName. Then it replaces that RoomName room in the old GameMap with the input Room.
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap roomname room mp =
    case M.lookup roomname mp of -- rname is the name of the room to be replaced
        Nothing -> mp -- room to be replaced doesn't exist in the map, just return the map
        Just _ -> M.insert (rname room) room (M.delete roomname mp)


-- setMessage is a handy utility function that takes a String and a GameState as an input, then
-- returns the same GameState with its message field replaced with the input string. You're
-- going to use this function to have the game put a message into its state to be displayed to the user.
setMessage :: String -> GameState -> GameState
setMessage msg gs = gs {message = Just msg} -- i get Just "" instead of Nothing CHECK!


-- This is a handy getter function. currentInventory takes a GameState as an input and returns the
-- inventory of that GameState's player.
currentInventory :: GameState -> [ItemName]
currentInventory gs = inventory $ player gs


-- currentRoom takes a GameState as an input, and returns whatever room the Player is
-- located in that game state.
currentRoom :: GameState -> Room
currentRoom gs = case M.lookup (location $ player gs) (gmap gs) of
    Nothing -> error "Invalid roomname"
    Just room -> room


-- nearbyObjects takes a game state as input and returns the list of item names in the
-- room where the player is, in that game state.
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gs = objects $ currentRoom gs


-- checks if an item is slippery
checkSlippery :: ItemName -> GameState -> Bool
checkSlippery itemName st =
    if slipperyMoves (getObject itemName st) /= Nothing then True else False

-- the function you use to have you, the player, pick up an item from a room you're in
-- and add it to your inventory.
takeItem :: ItemName -> GameState -> GameState
takeItem itemName st
  | elem itemName (currentInventory st) = st {message = Just $ "You are already carrying the " ++ show itemName ++ "."}
  | notElem itemName (nearbyObjects st) = st {message = Just $ "There is no " ++ show itemName ++ " in this room."}
  | ((weight (getObject itemName st)) + (inventoryWeight st)) > maxWeight (player st) = st {message = Just $ "That's too much weight for you to carry."}
  | otherwise =
        let item = getObject itemName st
            removedRoom = Room.removeItem itemName (currentRoom st)
            addedPlayer = Player.addItem itemName (player st)
            newRoom = setRoomMap (location (addedPlayer)) removedRoom (gmap st)
            newMap = setRoomMap (rname (currentRoom st)) removedRoom (gmap st)
            updatedItem =
                if slipperyMoves item /= Nothing
                    then item {slipperyMoves = numSlipperyMoves}
                    else item
        in st
            { message = Just $ "You take the " ++ show itemName ++ ".",
                gmap = newMap,
                player = addedPlayer
            }

-- takes a Direction and GameState as inputs, and if it is possible for the player to move in that direction,
-- returns a new game state with the player in the new location, and a message field
-- If it is not possible for the player to move in that direction, it returns the input game state, with another message
move :: Direction -> GameState -> GameState
move dir st =
    let nextRoom = destinationName dir (currentRoom st)
    in case nextRoom of
        Just roomName ->
            let nextRoom = getRoom roomName st
            in st
                { player = (player st) {location = roomName},
                    message = Just $ "You go " ++ (show dir) ++ "."
                }
            -- decrementSlipperyMoves function here
        Nothing -> st {message = Just "There is no exit in that direction."}


numSlipperyMoves :: Maybe Integer
numSlipperyMoves = Just 3

-- I really tried, but I couldn't figure it out! Specifically, I'm confused about how to go through each item
-- and update the gamestate. I think I have the correct code for one item, but I couldn't get it to work
-- with the player's inventory.

-- The idea here was to have a function to decrement all the slippery moves of slippery items in a player's inventory
decrementSlipperyMoves :: GameState -> GameState
decrementSlipperyMoves st = undefined

-- The idea here was to have a function to decrement the slippery moves of one item, and if the item has reached the max
-- num of slippery moves, then the item gets removed from the player's inventory and into the room and the gamestate is updated
-- But I couldn't figure out how to update the gamestate while going through each item, it got a bit messy so I just left my
-- progress for the code here!
decrementItemMoves :: GameState -> ItemName -> GameState
decrementItemMoves st itemName =
    let item = getObject itemName st
        isSlippery = checkSlippery itemName st
        updatedItem = if isSlippery then decrementMoves item else item
        updatedPlayer = Player.addItem (iname updatedItem) (player st)
    in if isSlippery && slipperyMoves updatedItem == Just 0
        then
            let updatedPlayer' = Player.removeItem itemName updatedPlayer
                updatedRoom = Room.addItem itemName (currentRoom st)
                newRoom = setRoomMap (location (updatedPlayer)) updatedRoom (gmap st)
                newMap = setRoomMap (location (updatedPlayer)) updatedRoom (gmap st)
            in st
                { message = Just $ "You feel something slip out of your pocket…",
                    gmap = newMap,
                    player = updatedPlayer'
                }
        else st {player = updatedPlayer}

-- Helper for decrementItemMoves, couldn't get this to work either
decrementMoves :: Item -> Item
decrementMoves item = undefined

-- decrementMoves :: Item -> Item
-- decrementMoves item =
--   let movesLeft = slipperyMoves item
--   in case movesLeft of
--     Just _ -> item {slipperyMoves = (Just movesLeft) - 1}
--     Nothing -> item

dropItem :: ItemName -> GameState -> GameState
dropItem itemName st
    | notElem itemName (currentInventory st) = st {message = Just $ "You are not carrying the " ++ show itemName ++ "."}
    | otherwise =
        let removedPlayer = Player.removeItem itemName (player st)
            addedRoom = Room.addItem itemName (currentRoom st)
            newRoom = setRoomMap (location (player st)) addedRoom (gmap st)
            newMap = setRoomMap (rname (currentRoom st)) addedRoom (gmap st)
        in st
            { message = Just $ "You drop the " ++ show itemName ++ ".",
                gmap = newMap,
                player = removedPlayer
            }

-- increases the max weight of a player
drink :: GameState -> GameState
drink st = if elem Gatorade (currentInventory st)
    then let newMax = 50
             removedPlayer = Player.removeItem Gatorade (player st)
         in st {message = Just $ "You drink the Gatorade and suddenly feel more energized.",
             player = removedPlayer {maxWeight = newMax}}
    else st {message = Just $ "You don't have anything to drink"}

-- Utility function that takes a GameState as input, then return the total weight of the
-- inventory that GameState's Player is carrying.
inventoryWeight :: GameState -> Integer
inventoryWeight st =
    let inv = inventory (player st) -- list of item names ["bucket", "lock"]
        ret = map (\name -> weight (getObject name st)) inv -- gets the weight of a single item from name
    in sum ret

-- takes a Direction and a Room as inputs, and if there is an exit in that direction in the input room,
-- it outputs the name of the room that you would end up in, if you walked out of the input room in
-- the input direction (wrapped in a Just). Otherwise, it outputs Nothing.
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir rm =
    let exitsMap = M.fromList $ exits rm
    in case M.lookup dir exitsMap of
        Just name -> Just name
        Nothing -> Nothing

-- ERROR HANDLING --

-- this defines a KeyError exception. When we want to throw that exception,
-- we'll type 'throw KeyError'.
data KeyError = KeyError
  deriving (Show)

instance Exception KeyError

-- checks if player is carrying an item
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck itemName st =
    if elem itemName (currentInventory st)
        then Left $ "You are already carrying the " ++ show itemName ++ "."
        else Right st

-- checks if item user is trying to pick up is actually there in the room
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck itemName st =
    if elem itemName (nearbyObjects st)
        then Right st
        else Left $ "There is no " ++ show itemName ++ " in this room."

-- checks if a player is able to carry a new item
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck itemName st =
    if ( (weight (getObject itemName st))
            + (inventoryWeight st)
        )
    > maxWeight (player st)
    then Left $ "That's too much weight for you to carry."
    else Right st

-- checks whether an item is in the player's inventory or in the room where the player is
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck itemName st =
    if ((elem itemName (currentInventory st)) || (elem itemName (nearbyObjects st)))
        then Left $ "What do you mean, drop the " ++ show itemName ++ "?"
        else Right st

-- checks whether an item is in the room where the player is
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck itemName st =
    if (elem itemName (nearbyObjects st))
        then Left $ "You aren't carrying the " ++ show itemName ++ "."
        else Right st
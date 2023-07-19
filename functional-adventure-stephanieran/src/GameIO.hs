module GameIO where

import Command
import Control.Monad.State
import Data.Map qualified as Map
import GameState
import Item
import Player
import Room
import System.Exit
import System.IO

-- 1.5.1 GameIO datatype
type GameIO = StateT GameState IO

-- 1.5.2 Prompt
prompt :: GameIO ()
prompt = liftIO $ putStr "-> " >> hFlush stdout

-- 1.5.3 printMessage
printMessage :: GameIO ()
printMessage = do
    gameState <- get
    case message gameState of
        Just value -> do
            liftIO $ putStrLn value
            put $ gameState {message = Nothing}
        Nothing -> return ()

-- 1.5.4 printDescription
printDescription :: GameIO ()
printDescription = do
    gameState <- get
    let loc = location (player gameState)
    case Map.lookup loc (gmap gameState) of
        Just room -> liftIO $ putStrLn (desc room)
        Nothing -> return ()

-- 1.5.5. printObjects
printObjects :: GameIO ()
printObjects = do
    gameState <- get
    let room = currentRoom gameState
        objectsInRoom = objects room
    when (not $ null objectsInRoom) $ do
        liftIO $ putStrLn "You see the following objects:"
        liftIO $ mapM_ (putStrLn . show) objectsInRoom

-- 2.0.1
printExits :: GameIO ()
printExits = do
    gameState <- get
    let room = currentRoom gameState
        exitsInRoom = map fst (exits room)
    when (not $ null exitsInRoom) $ do
        liftIO $ putStrLn "There are exits in the following directions:"
        liftIO $ mapM_ (putStrLn . show) exitsInRoom

-- 2.0.2
printInventory :: GameIO ()
printInventory = do
    gameState <- get
    let inventoryItems = inventory (player gameState)
    if null inventoryItems
        then liftIO $ putStrLn "You aren't carrying anything."
        else do
        liftIO $ putStrLn "You are carrying the following items:"
        liftIO $ mapM_ (putStrLn . show) inventoryItems

-- 2.0.3
actionOverList :: (ItemName -> GameState -> GameState) -> [ItemName] -> GameIO ()
actionOverList _ [] = return ()
actionOverList f (item : items) = do
    state <- get
    let updatedState = f item state
    liftIO $ putStrLn $ maybe "" id (message updatedState)
    put updatedState
    actionOverList f items

-- 2.0.4
finishGame :: GameIO ()
finishGame = do
    liftIO $ putStrLn "You successfully brought the mat into the Strength Training Room."
    liftIO $ putStrLn "Congrats! You win!"
    liftIO exitSuccess

-- 2.0.5
exit :: GameIO ()
exit = do
  liftIO $ putStrLn "Goodbye!"
  liftIO exitSuccess

-- 2.0.6
checkGameOver :: GameIO ()
checkGameOver = do
  gameState <- get
  if haveWonGame gameState then finishGame else return ()

-- 2.0.7
syntaxError :: GameIO ()
syntaxError = liftIO $ putStrLn "I don't understand that."

-- 2.0.8
opening :: GameIO ()
opening = liftIO $ putStrLn "Welcome to Functional Adventure!"

-- 2.0.9
performCommand :: Command -> GameIO ()
performCommand (Look) = printDescription >> printObjects >> printExits
performCommand (Move direction) = modify (move direction) >> printMessage
performCommand (Drink) = modify drink >> printMessage
performCommand (Inventory) = printInventory
performCommand (Take items) = actionOverList takeItem items
performCommand (Drop items) = actionOverList dropItem items
performCommand (Exit) = exit

-- 2.0.10
performConjunction :: Conjunction -> GameIO ()
performConjunction [] = return ()
performConjunction (command : commands) = do
    performCommand command
    performConjunction commands

-- 2.0.11
parseConjunction :: String -> GameIO ()
parseConjunction input = do
    let maybeConjunction = parseInput input
    case maybeConjunction of
        Just conjunction -> performConjunction conjunction
        Nothing -> syntaxError

repl :: GameIO ()
repl = do
  prompt
  input <- liftIO getLine
  parseConjunction input
  checkGameOver
  repl
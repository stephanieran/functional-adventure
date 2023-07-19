module Main where
import Control.Monad.State

import GameState
import GameIO

main :: IO ()
main = do
    putStrLn "Welcome to Functional Adventure!"
    evalStateT repl initialState
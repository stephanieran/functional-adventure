[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/BotLDWW8)
# functional-adventure
Stephanie Ran
5/25/2023

Notes:
- I couldn't figure out the slippery item feature after spending a really long time debugging
- I included my progress on the feature but currently the game doesn't have the feature implemented
- I included comments thoughout about how I would implement the feature, but here was the idea:
    - The idea here was to have two main functions-- first, one that goes through each item of a player's inventory, and second, a function to decrement the slippery moves of one item, and if the item has reached the max num of slippery moves, then the item gets removed from the player's inventory and into the room and the gamestate is updated
    - I was struggling to update the gamestate with each decrement of an item's slippery move
    - I gave it my best though and left what code I had written in there even if the feature wasn't working!
- The new feature I added is the "drink gatorade" feature. My game is set in a gym, and drinking the gatorade increases the max weight that the player can carry in their inventory
- The gatorade disappears from the inventory after the player drinks it
- To drink the gatorade, the gatorade has to be in the inventory, and the player has to use the command "drink"

Acknowledgements - Tarika Mane, Yamato Hart, Prof Teichman

## Build

```
stack build
```

## Test

```
stack test
```

## Run

```
stack run
```

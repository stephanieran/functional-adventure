module Direction where

data Direction = N | S | E | W deriving(Eq, Ord)

instance Show Direction where
    show N = "north"
    show S = "south"
    show E = "east"
    show W = "west"
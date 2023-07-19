module Item where

import Data.Map qualified as M

data ItemName
  = Bucket
  | Noodle
  | Towel
  | Lock
  | Dumbbell
  | Bench
  | Mat
  | Gatorade
  deriving (Eq, Ord)

instance Show ItemName where
    show name = case name of
        Bucket -> "bucket"
        Noodle -> "noodle"
        Towel -> "towel"
        Lock -> "lock"
        Dumbbell -> "dumbbell"
        Bench -> "bench"
        Mat -> "mat"
        Gatorade -> "gatorade"

type Universe = M.Map ItemName Item

data Item = Item
  { iname :: ItemName,
    weight :: Integer,
    slipperyMoves :: Maybe Integer
  }
  deriving (Show, Eq)

bucket :: Item
bucket = Item {iname = Bucket, weight = 15, slipperyMoves = Nothing}

noodle :: Item
noodle = Item {iname = Noodle, weight = 5, slipperyMoves = Just 3}

towel :: Item
towel = Item {iname = Towel, weight = 10, slipperyMoves = Nothing}

lock :: Item
lock = Item {iname = Lock, weight = 8, slipperyMoves = Nothing}

dumbbell :: Item
dumbbell = Item {iname = Dumbbell, weight = 25, slipperyMoves = Nothing}

bench :: Item
bench = Item {iname = Bench, weight = 40, slipperyMoves = Nothing} -- max weight

mat :: Item
mat = Item {iname = Mat, weight = 10, slipperyMoves = Nothing}

gatorade :: Item
gatorade = Item {iname = Gatorade, weight = 2, slipperyMoves = Nothing}

-- universe
univ :: Universe
univ = mkUniverse [bucket, noodle, towel, lock, dumbbell, bench, mat, gatorade]

items :: [Item]
items = [bucket, noodle, towel, lock, dumbbell, bench, mat, gatorade]

-- mkUniverse turns a list of Item-s into a Universe
mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList $ map (\item -> (iname item, item)) items
{- Maze solver - inspired by AOC 2024 Day 16 Gregor Pogacnik -}

import Data.Bifunctor (bimap)
import Data.List (minimumBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- Params
rotateCost = 1000

defaultShow = 3

-- Utilities
putStrLnErr = hPutStrLn stderr

-- Maze type definitions
type Maze = [[Char]]

type Position = (Int, Int)

-- Directions to move in the maze
newtype Direction = Direction (Int, Int) deriving (Eq, Ord)

directions :: [Direction]
directions = [Direction (0, 1), Direction (1, 0), Direction (0, -1), Direction (-1, 0)]

instance Show Direction where
  show (Direction (0, 1)) = "v"
  show (Direction (1, 0)) = ">"
  show (Direction (0, -1)) = "^"
  show (Direction (-1, 0)) = "<"
  show _ = "*"

-- Check if a position is within maze bounds and not a wall
isValid :: Maze -> Position -> Bool
isValid maze (x, y) =
  x >= 0 && y >= 0 && y < length maze && x < length (head maze) && maze !! y !! x /= '#'

-- Find the postion of a char in maze
findPosition :: Maze -> Char -> Maybe Position
findPosition maze target = findInRows (zip [0 ..] maze)
  where
    findInRows [] = Nothing
    findInRows ((r, row) : rows) =
      case findInRow row of
        Just c -> Just (c, r)
        Nothing -> findInRows rows

    findInRow row = lookup target (zip row [0 ..])

-- Find all paths using breadth-first-search
bfsAllPaths :: Maze -> Position -> Position -> [[Position]]
bfsAllPaths maze start goal = bfs' Set.empty [(start, [start])]
  where
    bfs' _ [] = [] -- No more paths to explore
    bfs' visited ((pos, path) : queue)
      | pos == goal = path : bfs' visited queue
      | otherwise =
          let neighbors = [(x + dx, y + dy) | Direction (dx, dy) <- directions, let (x, y) = pos]
              validNeighbors = filter (\n -> isValid maze n && not (n `elem` path)) neighbors
              newQueue = queue ++ [(n, path ++ [n]) | n <- validNeighbors]
           in bfs' visited newQueue

-- Distance
data S = S {point :: Position, multiplier :: Position, agg :: Int}

emptyS = S {point = (0, 0), multiplier = (0, 0), agg = 0}

dotProduct :: (Num a) => (a, a) -> (a, a) -> a
dotProduct (a, b) (c, d) = sum $ zipWith (*) [a, b] [c, d]

distance :: Position -> Position -> Position
distance = (\(a, b) (c, d) -> (abs (a - c), abs (b - d)))

manhattan :: Position -> Position -> Int
manhattan a b = let diff = distance a b in fst diff + snd diff

-- Core
changeVal :: Int -> Int -> Position -> Position
changeVal old new p =
  let fun val = if val == old then new else val
   in bimap fun fun p

pathCombine :: Int -> S -> Position -> S
pathCombine cost s p =
  let diff = distance (point s) p
      new = agg s + dotProduct diff (multiplier s)
      man = manhattan (point s) p
      mul = if man == 0 then (0, 0) else multiplier s
      override = cost + 1
   in case mul of
        (0, 0) -> S {point = p, multiplier = (1, override), agg = new} -- vertical move is immediately costly
        otherwise -> S {point = p, multiplier = changeVal 0 override diff, agg = new} -- when vector is 0 -> diff e.g., (0, 1) -> (1001, 1)

pathLength :: [Position] -> Int
pathLength positions =
  let combination = foldl (pathCombine rotateCost) emptyS positions
   in agg combination

-- Overlay on map
data C = C {lastp :: Maybe Position, res :: Map.Map Position Char}

emptyC = C {lastp = Nothing, res = Map.empty}

charCombine :: C -> Position -> C
charCombine s p =
  case lastp s of
    Nothing -> C {lastp = Just p, res = res s}
    Just o -> C {lastp = Just p, res = updateMap o p (res s)}
  where
    calcChar (x1, y1) (x2, y2) =
      let dist = Direction $ (x2 - x1, y2 - y1)
       in (show dist) !! 0
    -- updateMap is an overcomplication since you want border char to already be moved
    -- else you could just Map.insert p (calcChar o p) (res s)
    updateMap o p m =
      let newChar = calcChar o p
          oldChar = fromMaybe newChar (Map.lookup o m)
          m' = if oldChar /= newChar then (Map.insert o newChar m) else m
       in Map.insert p newChar m'

calcOverlay :: [Position] -> Map.Map Position Char
calcOverlay positions =
  let result = foldl charCombine emptyC positions
   in Map.delete (last positions) (res result) -- Delete last or you overwrite 'E'

overlay :: Maze -> Map.Map Position Char -> Maze
overlay maze overlay =
  [ [updateCell (x, y) cell | (x, cell) <- zip [0 ..] row]
    | (y, row) <- zip [0 ..] maze
  ]
  where
    updateCell pos cell =
      case Map.lookup pos overlay of
        Just c -> c
        Nothing -> cell

-- Find all elements with the minimum value
minimumByAll :: (Ord b) => (a -> b) -> [a] -> [a]
minimumByAll f xs = filter (\x -> f x == minValue) xs
  where
    minValue = f $ minimumBy (comparing f) xs

convert :: Maybe [a] -> Maybe [a]
convert (Just xs) = if null xs then Nothing else Just xs
convert Nothing   = Nothing

main :: IO ()
main = do
  input <- getContents
  let maze = lines input
  args <- getArgs
  let param = case args of
        (x : _) -> read x :: Int
        [] -> defaultShow

  let solution = do
        s <- findPosition maze 'S'
        e <- findPosition maze 'E'
        return (minimumByAll pathLength $ bfsAllPaths maze s e)

  case (convert solution) of
    Nothing -> putStrLnErr "Maze cannot be solved"
    Just some -> do
      if param > 0 then putStrLnErr "Some solutions: " else return ()
      mapM_ printMaze (take param some)
      putStrLnErr $ "Number of optimal solutions: " ++ show (length some)
      putStrLn $ show (pathLength (some !! 0))
      where
        printMaze x = mapM_ putStrLnErr $ overlay maze (calcOverlay x)

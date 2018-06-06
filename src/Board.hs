module Board where

import Control.Monad.Random
    ( Rand
    , RandomGen
    , getRandomR
    , getStdGen
    , runRand
    , split
    )
import Control.Monad
import Data.Map (Map, (!), fromList, insert, toList)

import Msg
import Pos
import Cell

w :: Int
w = 40

h :: Int
h = 30


type Board = Map Pos Cell

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0 :: Float, 1.0 :: Float)
    return $ Cell (t < 0.201) False False 0

initBoard :: RandomGen g => [Pos] -> Rand g Board
initBoard positions = 
    (fromList . zip positions) <$> replicateM (length positions) mkCell

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
    initBoard positions

adjacents :: Pos -> [Pos]
adjacents (x, y) =
    [ (xx, yy)
    | xx <- [x - 1 .. x + 1]
    , yy <- [y - 1 .. y + 1]
    , (xx, yy) /= (x, y)
    , xx >= 0
    , yy >= 0
    , xx < w
    , yy < h
    ]

exposeMines :: Board -> Board
exposeMines board = 
    newBoard where
        toExpose = filter (\(pos, cell) -> (not . exposed) cell && mined cell) $ toList board
        modificated = fmap (\(p, c) -> (p, Just $ c {exposed = True})) toExpose
        newBoard = foldl (\b (p, Just c) -> insert p c b) board modificated

exposeSelection :: Board -> Pos -> Cell -> Int -> Board
exposeSelection board pos cell count =
    newBoard where
        cell = board ! pos
        toExpose = if flagged cell
                        then []
                        else [(pos, cell)]
        modificated = fmap (\(p, c) -> (p, Just $ c {exposed = True, mineCount = count})) toExpose
        newBoard = foldl (\b (p, Just c) -> insert p c b) board modificated

exposeCells :: Board -> Pos -> Board
exposeCells board pos =
    newBoard where
        cell@(Cell m e f mc) = board ! pos
        indices = adjacents pos
        count = length $ filter mined $ fmap (board !) indices
        checkList = if m || e || f || count /= 0
                        then []
                        else indices
        b1 = exposeSelection board pos cell count
        b2 = foldl (\b -> exposeCells b) b1 checkList
        b3 = if m
                then exposeMines b2
                else b2
        newBoard = b3

flagCell :: Board -> Pos -> Board
flagCell board pos =
    newBoard where
        cell = board ! pos
        modificated = if exposed cell
                        then []
                        else [(pos, Just $ cell {flagged = not $ flagged cell})]
        newBoard = foldl (\b (p, Just c) -> insert p c b) board modificated

gameOver :: Board -> Bool
gameOver = any (\cell -> exposed cell && mined cell)

updateBoard :: Msg -> Board -> Board
updateBoard msg board = 
    if gameOver board
        then board
        else case msg of
            LeftPick pos -> exposeCells board pos
            RightPick pos -> flagCell board pos

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Random (Rand, getRandom, getRandomR, runRand, evalRand)
import Control.Monad.State (execState)
import Data.Map (Map, (!), fromList, insert, mapWithKey, toList)
import System.Random

import Miso
import Miso.String (MisoString, ms, pack)
import Miso.Svg hiding (height_, style_, width_)

import Consts
import Board
import Cell
import Msg
import Smiley

type Seed = Int
type Game = (Board, Seed)


centerStyle :: Map MisoString MisoString
centerStyle =
    fromList [("width", "75%"), ("margin", "0 auto"), ("text-align", "center")]

viewGame :: Game -> View Msg
viewGame (board, _) =
    div_
        []
        [ div_ [style_ centerStyle] (showFace (gameOver board))
        , div_ [style_ centerStyle] [text "Implemented using Miso"]
        , div_
              [style_ centerStyle]
              [ svg_
                    [ version_ "1.1"
                    , width_ (ms $ show (gridWidth * cellSize))
                    , height_ (ms $ show (gridHeight * cellSize))
                    ]
                    (map snd (toList (mapWithKey showCell board)))
              ]
        , div_ [style_ centerStyle] [button_ [onClick Reset] [text "reset"]]
        ]

updateGame :: Msg -> Game -> Effect Msg Game
updateGame msg (board, seed) =
    case msg of
        Reset ->
            noEff (newBoard, newSeed)
            where
                g0 = mkStdGen seed
                (newBoard, g1) = runRand mkBoard g0
                newSeed = evalRand getRandom g1
        _ ->
            noEff (newBoard, seed)
            where
                newBoard = updateBoard msg board

main :: IO ()
main = do
    seed <- getStdRandom random
    let
        initialAction = Reset
        model = (mempty, seed)
        update = updateGame
        view = viewGame
        events = Data.Map.insert "contextmenu" False defaultEvents
        subs = []
        mountPoint = Nothing
    startApp App {..}

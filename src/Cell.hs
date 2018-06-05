{-# LANGUAGE OverloadedStrings #-}

module Cell
    ( Cell (..)
    , showCell
    , cellSize
    ) where


import Pos
import RightClick

import Msg

import Data.Map (Map, fromList)
import Miso
import Miso.String (MisoString, ms, pack)
import Miso.Svg hiding (height_, style_, width_)


data Cell = Cell
    { mined :: Bool
    , exposed :: Bool
    , flagged :: Bool
    , mineCount :: Int
    } deriving (Show, Eq)


cellSize :: Int
cellSize = 20

showFlag :: Pos -> [View Msg]
showFlag pos =
    [ polygon_
          [ points_ "0.20,0.40 0.70,0.55 0.70,0.25"
          , fill_ "red"
          , onClick (LeftPick pos)
          , onRightClick (RightPick pos)
          ]
          []
    , line_
          [ x1_ "0.70"
          , y1_ "0.25"
          , x2_ "0.70"
          , y2_ "0.85"
          , strokeWidth_ ".07"
          , stroke_ "black"
          , onClick (LeftPick pos)
          , onRightClick (RightPick pos)
          ]
          []
    ]

showMine :: Pos -> [View Msg]
showMine pos =
    [ polygon_
          [points_ "0.65,0.15 0.85,0.35 0.65,0.55 0.45,0.35 ", fill_ "brown"]
          []
    , circle_
          [ cx_ "0.45"
          , cy_ "0.55"
          , r_ "0.3"
          , fill_ "brown"
          , onClick (LeftPick pos)
          , onRightClick (RightPick pos)
          ]
          []
    ]

showSquare :: Pos -> Cell -> View Msg
showSquare pos cell =
    rect_
        [ x_ "0.05"
        , y_ "0.05"
        , width_ "0.9"
        , height_ "0.9"
        , style_ $ fromList [("fill", ms $ getColor cell)]
        , onClick (LeftPick pos)
        , onRightClick (RightPick pos)
        ]
        []

showText :: Pos -> Int -> [View Msg]
showText pos count =
    let textColor =
            case count of
                1 -> "blue"
                2 -> "green"
                3 -> "red"
                4 -> "brown"
                _ -> "purple"
    in [ text_
             [ x_ "0.5"
             , y_ "0.87"
             , fontSize_ "1.0"
             , fill_ textColor
             , textAnchor_ "middle"
             , onClick (LeftPick pos)
             , onRightClick (RightPick pos)
             ]
             [text $ ms $ show count]
       ]

getColor :: Cell -> String
getColor (Cell _ exposed _ _) =
    if exposed
        then "#909090"
        else "#CCCCCC"

showCellDetail :: Pos -> Cell -> [View Msg]
showCellDetail pos (Cell mined exposed flagged mineCount) =
    case ( flagged,    mined, exposed, 0 /= mineCount) of
         (    True,       _,       _,       _) -> showFlag pos
         (       _,    True,    True,       _) -> showMine pos
         (       _,       _,    True,    True) -> showText pos mineCount
         (       _,       _,       _,       _) -> []

showCell :: Pos -> Cell -> View Msg
showCell pos cell =
    g_ [ transform_
            (ms $    "scale (" ++ scale ++ ", " ++ scale ++ ") " ++ "translate (" ++ show x ++ ", " ++ show y ++ ") ")
       ]
       (showSquare pos cell : showCellDetail pos cell)
    where 
        (x, y) = pos
        scale = show cellSize
    

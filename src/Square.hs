{-# LANGUAGE OverloadedStrings #-}

module Square
    ( showSquare
    ) where

import Data.Map (fromList)
import Miso
import Miso.String (MisoString, ms, pack)
import Miso.Svg hiding (height_, id_, style_, width_)

import Msg
import Pos
import Cell
import RightClick


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

getColor :: Cell -> String
getColor (Cell _ exposed _ _) =
    if exposed
        then "#909090"
        else "#CCCCCC"

        
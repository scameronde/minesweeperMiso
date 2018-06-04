{-# LANGUAGE OverloadedStrings #-}

module Number
    ( showText
    ) where

import Miso
import Miso.String (MisoString, ms, pack)
import Miso.Svg hiding (height_, id_, style_, width_)

import Msg
import Pos
import RightClick


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

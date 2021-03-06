{-# LANGUAGE OverloadedStrings #-}

module Smiley
    ( showFace
    ) where

import Data.Map

import Miso hiding (height_, style_, width_)
import Miso.String (pack)
import Miso.Svg
    ( circle_
    , cx_
    , cy_
    , d_
    , fill_
    , g_
    , height_
    , path_
    , r_
    , strokeWidth_
    , stroke_
    , style_
    , svg_
    , transform_
    , width_
    )

import Msg

showFace :: Bool -> [View Msg]
showFace lost =
    let sz = 100
    in [ svg_
             [width_ "100", height_ "100"]
             [ g_ [ transform_
                        (pack $
                         "scale (" ++
                         show sz ++
                         ", " ++ show sz ++ ") " ++ "translate (0.5, 0.5)")
                   ]
                    -- face outline
                   ([ circle_
                          [ cx_ "0.0"
                          , cy_ "0.0"
                          , r_ "0.4"
                          , fill_ "yellow"
                          , stroke_ "black"
                          , strokeWidth_ "0.02"
                          ]
                          []
                    ] ++ -- eyes
                    fmap
                        (\xc ->
                             circle_
                                 [ cx_ (pack $ show xc)
                                 , cy_ "-0.1"
                                 , r_ "0.08"
                                 , fill_ "yellow"
                                 , stroke_ "black"
                                 , strokeWidth_ "0.02"
                                 ]
                                 [])
                        [0.15, -0.15] ++ -- smile/frown
                    [ path_
                          [ d_ (pack
                                    ("M-0.15,0.15 a0.2,0.2 0 0 " ++
                                     (if lost
                                          then "1"
                                          else "0") ++
                                     " 0.30,0.0"))
                          , stroke_ "black"
                          , strokeWidth_ "0.02"
                          , fill_ "none"
                          ]
                          []
                    ] ++
                    if lost
                          -- eye crosses
                        then let param =
                                     [ (ex, dx, dy)
                                     | ex <- [-0.15, 0.15]
                                     , dx <- [-0.1, 0.1]
                                     , dy <- [-0.1, 0.1]
                                     ]
                             in fmap
                                    (\(ex, dx, dy) ->
                                         path_
                                             [ d_ $
                                               pack
                                                   ("M " ++
                                                    show ex ++
                                                    " -0.1 l " ++
                                                    show dx ++ " " ++ show dy)
                                             , stroke_ "black"
                                             , strokeWidth_ "0.02"
                                             , fill_ "none"
                                             ]
                                             [])
                                    param
                          -- eyeballs
                        else fmap
                                 (\xc ->
                                      circle_
                                          [ cx_ (pack $ show xc)
                                          , cy_ "-0.1"
                                          , r_ "0.04"
                                          , fill_ "black"
                                          ]
                                          [])
                                 [0.15, -0.15])
             ]
       ]

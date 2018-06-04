{-# LANGUAGE OverloadedStrings #-}

module Cell
    ( Cell (..)
    ) where


data Cell = Cell
    { mined :: Bool
    , exposed :: Bool
    , flagged :: Bool
    , mineCount :: Int
    } deriving (Show, Eq)


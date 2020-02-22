{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Lib

type BoardIndex = Int
data Piece = Maru | Batsu | None
type Board = [(Int, Piece)]

main :: IO ()
main = someFunc

initBoard :: BoardIndex -> Board
initBoard 0 = [(0, None)]
initBoard index = [(index, None)] ++ initBoard (index - 1)

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Lib

type BoardIndex = Int
data Piece = Maru | Batsu | None deriving Eq
type Board = [(BoardIndex, Piece)]

main :: IO ()
main = someFunc

initBoard :: BoardIndex -> Board
initBoard 0 = [(0, None)]
initBoard index = [(index, None)] ++ initBoard (index - 1)

markBoard :: Board -> BoardIndex -> Piece -> Board
markBoard board index piece = map (markBoardUnit index piece) board

markBoardUnit :: BoardIndex -> Piece -> (BoardIndex, Piece) ->  (BoardIndex, Piece)
markBoardUnit index piece unit
    | index == fst unit = (index, piece)
    | otherwise = unit

isFinished :: Board -> Bool
isFinished board = not $ any ((==None).snd) board
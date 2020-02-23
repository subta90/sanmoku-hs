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

extractIndexList :: Board -> Piece -> [BoardIndex]
extractIndexList board piece = map fst $ filter ((==piece).snd) board

judgeWinner :: Board -> Piece
judgeWinner board
    | isLineCompleted $ extractIndexList board Maru = Maru
    | isLineCompleted $ extractIndexList board Batsu = Batsu
    | otherwise = None

isLineCompleted :: [BoardIndex] -> Bool 
isLineCompleted indexList
    | isRowLineCompleted indexList = True
    | isColumnLineCompleted indexList = True
    | isDiagonallyLineCompleted indexList = True
    | otherwise = False

isRowLineCompleted :: [BoardIndex] -> Bool
isRowLineCompleted [0,1,2] = True
isRowLineCompleted [3,4,5] = True
isRowLineCompleted [6,7,8] = True
isRowLineCompleted _ = False

isColumnLineCompleted :: [BoardIndex] -> Bool
isColumnLineCompleted [0,3,6] = True
isColumnLineCompleted [1,4,7] = True 
isColumnLineCompleted [2,5,8] = True
isColumnLineCompleted _ = False

isDiagonallyLineCompleted :: [BoardIndex] -> Bool
isDiagonallyLineCompleted [0,4,8] = True
isDiagonallyLineCompleted [2,4,6] = True
isDiagonallyLineCompleted _ = False


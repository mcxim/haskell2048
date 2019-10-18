module Lib where

{- 
My simple console implementation of the game 2048 in haskell, just to
grow more farmiliar with the language.
-}

import           Data.List
import           Prelude                 hiding ( Left
                                                , Right
                                                )
import           System.Random                  ( randomRIO )
import           Data.Char                      ( toLower )
import           Control.Monad                  ( (>=>) )


data Move = Lft | Rgt | Upp | Dwn

type Board = [[Int]]

nCols = 4 :: Int
nRows = 4 :: Int

changePos :: Int -> Int -> Int -> Board -> Board
changePos row col val board =
  take row board
    ++ [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

checkWin :: Board -> Bool
checkWin = any (elem 2048)

checkLose :: Board -> Bool
checkLose board =
  (not . any (elem 0)) board
    && move Lft board
    == board
    && move Rgt board
    == board
    && move Upp board
    == board
    && move Dwn board
    == board

merge :: [Int] -> [Int]
merge lst = take (length lst) $ merge' (filter (/= 0) lst) ++ repeat 0

merge' :: [Int] -> [Int]
merge' []  = []
merge' [a] = [a]
merge' (x : y : xs) | x == y    = (2 * x) : merge' xs
                    | otherwise = x : merge' (y : xs)

move :: Move -> Board -> Board
move Lft = map merge
move Rgt = map (reverse . merge . reverse)
move Upp = transpose . move Lft . transpose
move Dwn = transpose . move Rgt . transpose

myShow :: Int -> String
myShow num | len == 1 = "  " ++ shown ++ "  "
           | len == 2 = " " ++ shown ++ "  "
           | len == 3 = " " ++ shown ++ " "
           | len == 4 = shown ++ " "
 where
  shown = if num == 0 then "_" else show num
  len   = length shown

prettyPrint :: Board -> IO ()
prettyPrint =
  putStrLn . unlines . map unwords . intersperse [] . (map . map) myShow

emptyBoard = [ [ 0 | col <- [1 .. nCols] ] | row <- [1 .. nRows] ] :: Board

addRandom :: Board -> IO Board
addRandom board =
  let emptyCells =
          [ (row, col)
          | row <- [0 .. nRows - 1]
          , col <- [0 .. nCols - 1]
          , board !! row !! col == 0
          ]
      chosenOne =
          randomRIO (0, length emptyCells - 1)
            >>= (\idx -> return $ emptyCells !! idx)
  in  if null emptyCells
        then return board
        else do
          random <- randomRIO (1 :: Int, 10)
          let val = if random == 10 then 4 else 2
          (row, col) <- chosenOne
          return $ changePos row col val board

newBoard :: IO Board
newBoard = (addRandom >=> addRandom) emptyBoard


gameLoop :: Board -> IO ()
gameLoop board
  | checkWin board = putStrLn "You won! congrats."
  | checkLose board = putStrLn "You lost, no more possible moves."
  | otherwise = do
    nextMove <- getLine
    let moved = case toLower (head nextMove) of
          'h' -> move Lft board
          'a' -> move Lft board
          'j' -> move Dwn board
          's' -> move Dwn board
          'k' -> move Upp board
          'w' -> move Upp board
          'l' -> move Rgt board
          'd' -> move Rgt board
          'q' -> []
          _   -> board
    if moved == board
      then gameLoop board
      else if null moved
        then return ()
        else do
          nextBoard <- addRandom moved
          prettyPrint nextBoard
          gameLoop nextBoard

runGame :: IO ()
runGame = do
  putStrLn "Use hjkl or wasd keys to operate, q to quit."
  board <- newBoard
  prettyPrint board
  gameLoop board

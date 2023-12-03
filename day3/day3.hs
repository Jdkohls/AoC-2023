import Data.Char ( isDigit )
import Data.List
import GHC.Driver.Pipeline (preprocess)
import Data.List.Split
import Control.Monad

import Debug.Trace

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . sum . preprocessParse $ contents

debug :: c -> String -> c
debug = flip trace

getSymbols :: [Char] -> [Char] --   +%=#/@$&*-
getSymbols xs = getSymbols' [] xs
    where
        getSymbols' :: [Char] -> [Char] -> [Char]
        getSymbols' xs [] = xs
        getSymbols' li (x:xs) = if x `elem` ['.','\n'] || isDigit x || x `elem` li then getSymbols' li xs else getSymbols' (x:li) xs

getSymbolIndex :: [Char] -> [Char] -> [Int]
getSymbolIndex sym = reverse . getSymbolIndex' 0 []
    where
        getSymbolIndex' :: Int -> [Int] -> [Char] -> [Int]
        getSymbolIndex' i is (x:xs) = if x `elem` sym then getSymbolIndex' (i+1) (i:is) xs else getSymbolIndex' (i+1) is xs
        getSymbolIndex' _ is [] = is

getLineLength :: Int
getLineLength = length "...............................................657..........................423..........742.367...............................634.........."
--getLineLength = length "467..114.."

getSymbolNums :: [Char] -> [Char] -> [Int]
getSymbolNums xs sym = sort $ pm getLineLength $ pm 1 $ getSymbolIndex sym xs
    where
        pm :: Int -> [Int]  -> [Int]
        pm _ []  = []
        pm i (x:xs) = (x - i):x:(x+i):pm i xs


getNumIndexRange :: [Char] -> [[Int]]
getNumIndexRange = reverse . getNumIndexRange' 0 [] []
    where
        getNumIndexRange' :: Int -> [Char] -> [[Int]] -> [Char] -> [[Int]]
        getNumIndexRange' i xs acc (inp:inps)
          | isDigit inp = getNumIndexRange' (i+1) (inp:xs) acc inps
          | xs /= [] = getNumIndexRange' (i+1) [] ([read (reverse xs) :: Int, i - length xs, i-1]:acc) inps
          | otherwise = getNumIndexRange' (i+1) xs acc inps
        getNumIndexRange' i xs acc [] = if xs /= [] then [read (reverse xs) :: Int, i - length xs, i-1]:acc else acc

parse :: [Char] -> [Int]
parse xs = fn (getNumIndexRange xs) (getSymbolNums xs $ getSymbols xs) --`debug` (( show $ getNumIndexRange xs ) ++ "\n" ++ ( show $ getSymbolNums xs ))
    where
        fn :: [[Int]] -> [Int] -> [Int]
        fn [] _ = []
        fn _ [] = []
        fn ([val, l, h]:xs) (y:ys)
            | l > y            = fn ([val, l, h]:xs) ys -- goto next valid symbol space
            | h < y            = fn xs (y:ys)  --`debug` (" " ++ show y  ++ " " ++ show h ++ "\n") -- goto next num 
            | otherwise        = val : fn xs ys --`debug` "next"

preprocessParse :: [Char] -> [Int]
preprocessParse = parse . join . splitOn "\n"

getStars = getSymbolIndex "*" 

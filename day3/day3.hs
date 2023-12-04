import Data.Char ( isDigit )
import Data.List
import GHC.Driver.Pipeline (preprocess)
import Data.List.Split
import Control.Monad

import Debug.Trace

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . sum . parse . cleanup $ contents
    print . sum . parse' . cleanup $ contents

debug :: c -> String -> c
debug = flip trace

cleanup :: [Char] -> [Char]
cleanup = join . splitOn "\n"

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




getStars :: [Char] -> [[Int]]
getStars xs = map (sort . pm getLineLength) (pm' 1 (getSymbolIndex "*" xs))
    where
        pm' :: Int -> [Int]  -> [[Int]]
        pm' _ []  = []
        pm' i (x:xs) = [x - i, x, x + i]:pm' i xs

        pm :: Int -> [Int]  -> [Int]
        pm _ []  = []
        pm i (x:xs) = (x - i):x:(x+i):pm i xs

starmap :: [Char] -> [Int]
starmap = flip getSymbolNums "*"

findNearbyStars :: [Char] -> [[Int]]
findNearbyStars xs = fn (getNumIndexRange xs) (starmap xs)
    where
        fn :: [[Int]] -> [Int] -> [[Int]]
        fn [] _ = []
        fn _ [] = []
        fn xp@([val, l, h]:xs) yp@(y:ys)
            | l > y            = fn xp ys -- goto next valid symbol space
            | h < y            = fn xs yp  --`debug` (" " ++ show y  ++ " " ++ show h ++ "\n") -- goto next num 
            | otherwise        = [y,val] : fn xs ys


fltr :: [[Int]] -> [Int]
fltr [] = []
fltr xs = fltr' False 0 (-1) [] xs
    where
        fltr' _ _ _ acc [] = acc
        fltr' polarity num seen acc xp@([star,val]:xs)   
            | star == seen && polarity = (num*val):(fltr xs)
            | otherwise                = fltr' True val star acc xs


findTrue :: (a -> [a] -> Bool) -> a -> [[a]] -> Int
findTrue f cond li = findTrue' f cond li 0
    where
        findTrue' f cond (x:xs) i = if f cond x then i else findTrue' f cond xs (i+1)
        findTrue' f cond [] _ = (-1)



parse' :: [Char] -> [Int]
parse' [] = []
parse' xs =  fltr (sortBy (\[a,_] [b,_] -> compare a b) (findStar (findNearbyStars xs) (getStars xs))) 
    where
        findStar :: [[Int]] -> [[Int]] -> [[Int]]
        findStar _ [] = []
        findStar [] _ = []
        findStar ([starIndex, val]:xs) ys = [findTrue elem starIndex ys, val]:findStar xs ys



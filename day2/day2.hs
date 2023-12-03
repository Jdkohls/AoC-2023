import Data.List.Split
import Data.List
import Data.Bool (bool)
import Data.Ord

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . sum  $ map parse $ splitOn "\n" contents
    print . sum  $ map parse' $ splitOn "\n" contents

parse :: [Char] -> Int
parse input = if verify 12 13 14 input then getID input else 0

verify :: Int -> Int -> Int -> [Char] -> Bool
verify r g b input = all (fn r g b) (getColors input)
    where
        fn :: Int -> Int -> Int -> [Int] -> Bool
        fn r g b [red, green, blue] = red <= r && green <= g && blue <= b
        fn r g b _ = False

parse' :: [Char] -> Int
parse' input = product (minlist . getColors $ input)

minlist :: [[Int]] -> [Int]
minlist (x:xs) = minlist' x xs
    where
        minlist' [r,g,b] [] = [r,g,b]
        minlist' [r,g,b] ([r',g',b']:xs) = minlist' [max r r', max g g', max b b'] xs 
        
getColors :: [Char] -> [[Int]]
getColors input = map (fn . map tail . splitOn ",") (tail (splitOneOf ":;" input))
    where
        fn :: [[Char]] -> [Int]
        fn list = [color "red" list, color "green" list, color "blue" list]
        color :: [Char] -> [[Char]] -> Int
        color _ [] = 0
        color c (li:list)
            | c `isSuffixOf` li = read (head . words $ li) :: Int
            | otherwise = color c list

getID :: [Char] -> Int
getID input = read (head (tail (splitOneOf ": " input))) :: Int
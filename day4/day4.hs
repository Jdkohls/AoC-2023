import Data.List.Split (splitOn, splitOneOf)
import Debug.Trace (trace)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . sum . map parse $ splitOn "\n" contents -- Doesn't work if there are whitespaces besides \n
    print . partTwo $ splitOn "\n" contents -- Doesn't work if there are whitespaces besides \n

debug :: c -> String -> c
debug = flip trace

--numsParse :: [char] -> Int
numsParse :: [Char] -> [[[Char]]]
numsParse [] = []
numsParse xs = map (filter (not . null) . splitOn " ") (tail (splitOneOf ":|" xs))

countWins :: [[[Char]]] -> Int
countWins [] = 0
countWins [winning, pulls] = length (filter id (fn winning pulls))
    where
        fn :: [[Char]] -> [[Char]] -> [Bool]
        fn w (x:xs) =  elem x w : fn w xs
        fn _ [] = []
countWins xs = 0 `debug` show xs


ticketsWon :: [Char] -> Int
ticketsWon = countWins . numsParse

parse :: [Char] -> Int
parse = flip div 2 . (2 ^) . ticketsWon -- if 0 wins, 1 `div` 2 == 0; else reduces to 2^{n-1}
-- parse xs = div (2 ^ (countWins . numsParse  $ xs)) 2 

winlist :: [[Char]] -> [Int]
winlist = map ticketsWon


partTwo :: [[Char]] -> Int
partTwo =  sum . cleanup  . solve . zip  . winlist
    where
        zip :: [Int] -> [(Int,Int)]
        zip (x:xs) = (1,x):zip xs
        zip [] = []

        solve :: [(Int, Int)] -> [(Int, Int)]
        solve ((x,y):xs) = (x,y): solve (update x y xs)
        solve [] = []

        update :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
        update i 0 xs = xs
        update i j ((x,y):xs) = (x + i, y) : update i (j-1) xs
        update _ _ [] = []

        cleanup :: [(Int,Int)] -> [Int]
        cleanup [] = []
        cleanup ((x,_):xs) = x:cleanup xs
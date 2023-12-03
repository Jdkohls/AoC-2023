import Data.Char ( isDigit )

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . getSymbols $ contents -- Doesn't work if there are whitespaces besides \n

getSymbols :: [Char] -> [Char] --   +%=#/@$&*-
getSymbols xs = getSymbols' [] xs
    where
        getSymbols' :: [Char] -> [Char] -> [Char]
        getSymbols' xs [] = xs
        getSymbols' li (x:xs) = if x `elem` ['.','\n'] || isDigit x || x `elem` li then getSymbols' li xs else getSymbols' (x:li) xs 
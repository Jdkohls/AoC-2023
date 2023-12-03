import Data.Char ( isDigit )
import Data.List ( isPrefixOf  )

strtoint :: [Char] -> Int
strtoint [] = 0
strtoint xs = read [readfront xs, readfront . reverse $ xs] :: Int
    where
        readfront :: [Char] -> Char
        readfront [] = '0'
        readfront (y:ys) = if isDigit y then y else readfront ys

wordToDigit :: [Char] -> Char
wordToDigit n =
    case n of
        "zero" -> '0'
        "one" -> '1'
        "two" -> '2'
        "three" -> '3'
        "four" -> '4'
        "five" -> '5'
        "six" -> '6'
        "seven" -> '7'
        "eight" -> '8'
        "nine" -> '9'
        _ -> '0'

strtoint' :: [Char] -> Int
strtoint' [] = 0
strtoint' xs = read [readfront xs, readback xs] :: Int
    where
        readfront :: [Char] -> Char
        readfront [] = '0'
        readfront (y:ys)
            | isDigit y = y
            | isWord (y:ys) = if any (`isPrefixOf` (y:ys)) ["one","two","six"] then wordToDigit . take 3 $ y:ys else
                              if any (`isPrefixOf` (y:ys)) ["zero","four","five","nine"] then wordToDigit . take 4 $ y:ys else
                              if any (`isPrefixOf` (y:ys)) ["three","seven","eight"] then wordToDigit . take 5 $ y:ys else '0'
            | otherwise = readfront ys
        readback :: [Char] -> Char
        readback [] = '0'
        readback ys
            | isDigit (last ys) = last ys
            | isReverseWord (reverse ys) = if any ((`isPrefixOf` reverse ys) . reverse) ["one","two","six"] then wordToDigit . reverse . take 3 $ reverse  ys else
                              if any ((`isPrefixOf` reverse ys) . reverse) ["zero","four","five","nine"] then wordToDigit . reverse . take 4 $ reverse  ys else
                              if any ((`isPrefixOf` reverse ys) . reverse) ["three","seven","eight"] then wordToDigit . reverse . take 5 $ reverse  ys else '0'
            | otherwise = readback (init ys)

isWord :: [Char] -> Bool
isWord [] = False
isWord ys = any (`isPrefixOf` ys) ["zero","one","two","three","four","five","six","seven","eight","nine"]

isReverseWord :: [Char] -> Bool
isReverseWord [] = False
isReverseWord ys = any ((`isPrefixOf` ys) . reverse) ["one","two","three","four","five","six","seven","eight","nine"]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print . sum  $ map strtoint $ words contents -- Doesn't work if there are whitespaces besides \n
    print . sum  $ map strtoint' $ words contents -- Doesn't work if there are whitespaces besides \n

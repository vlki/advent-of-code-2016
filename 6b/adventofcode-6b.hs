import Data.List
import Data.Ord

leastCommonChar :: [Char] -> Char
leastCommonChar = last . last . sortBy (flip $ comparing length) . group . sort

decodeCharOnIndex :: [String] -> Int -> Char
decodeCharOnIndex messages index = leastCommonChar charsOnIndex
    where charsOnIndex = map (\message -> message !! index) messages

decodeMessage :: [String] -> String
decodeMessage messages = map (\index -> decodeCharOnIndex messages index) [0..maxIndex]
    where messageLength = length (messages !! 0)
          maxIndex = messageLength - 1

main = do
    input <- getContents
    putStrLn $ decodeMessage $ lines input

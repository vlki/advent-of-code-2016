import Text.Regex.Posix
import qualified Data.Map as Map
import Data.Char
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as LB

nextPasswordChar :: String -> Integer -> Integer -> Char
nextPasswordChar doorId position index = if isMatchingHash then (hash !! 6) else nextPasswordChar doorId position (index + 1)
    where hash = show $ md5 $ LB.pack (doorId ++ show index)
          isMatchingHash = (take 5 hash) == "00000" && toInteger (digitToInt (hash !! 5)) == position

doorPassword :: String -> Integer -> String
doorPassword _ 0 = ""
doorPassword doorId charsNum = char:(doorPassword doorId (charsNum - 1))
    where char = nextPasswordChar doorId (8 - charsNum) 0

main = do
    input <- getContents
    putStrLn $ doorPassword (head $ lines input) 8

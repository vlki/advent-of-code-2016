import Text.Regex.Posix
import qualified Data.Map as Map
import Data.Char
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as LB

nextPasswordChar :: String -> Integer -> (Char, Integer)
nextPasswordChar doorId index = if isMatchingHash then ((hash !! 5), index) else nextPasswordChar doorId (index + 1)
    where hash = show $ md5 $ LB.pack (doorId ++ show index)
          isMatchingHash = (take 5 hash) == "00000"

doorPassword :: String -> Integer -> Integer -> String
doorPassword _ 0 _ = ""
doorPassword doorId charsNum startIndex = char:(doorPassword doorId (charsNum - 1) (endIndex + 1))
    where (char, endIndex) = nextPasswordChar doorId startIndex

main = do
    input <- getContents
    putStrLn $ doorPassword (head $ lines input) 8 0

import Text.Regex.PCRE
import Data.List

abasFromSequence :: String -> [String]
abasFromSequence "" = []
abasFromSequence ipSequence = nub (matches ++ abasFromSequence (tail ipSequence))
    where matches = getAllTextMatches $ ipSequence =~ "([a-z])(?!\\1)[a-z]\\1" :: [String]

abasFromSequences :: [String] -> [String]
abasFromSequences sequences = nub $ concat $ map abasFromSequence sequences

hasBabInSequences :: String -> [String] -> Bool
hasBabInSequences bab sequences = or $ map (\ipSequence -> (ipSequence =~ bab :: Bool)) sequences

hasSomeBabInSequences :: [String] -> [String] -> Bool
hasSomeBabInSequences babs sequences = or $ map (\bab -> hasBabInSequences bab sequences) babs

aba2bab :: String -> String
aba2bab aba = (aba !! 1):(aba !! 0):(aba !! 1):""

supernetSequences :: String -> [String]
supernetSequences ip = cleanedMatches
    where rawMatches = getAllTextMatches $ ip =~ "[a-z]+\\[|\\][a-z]+\\[|\\][a-z]+" :: [String]
          cleanedMatches = map (\match -> filter (\c -> c /= '[' && c /= ']') match) rawMatches

hypernetSequences :: String -> [String]
hypernetSequences ip = cleanedMatches
    where rawMatches = getAllTextMatches $ ip =~ "\\[[^\\[]+\\]" :: [String]
          cleanedMatches = map (\match -> filter (\c -> c /= '[' && c /= ']') match) rawMatches

isIpWithSsl :: String -> Bool
isIpWithSsl ip = hasSomeBabInSequences babs (hypernetSequences ip)
    where supernetAbas = abasFromSequences $ supernetSequences ip
          babs = map aba2bab supernetAbas


countIpsWithSsl :: [String] -> Int
countIpsWithSsl ips = length ipsWithSsl
    where ipsWithSsl = filter isIpWithSsl ips

main = do
    input <- getContents
    putStrLn $ show $ countIpsWithSsl $ lines input

-- main = do
--     input <- getContents
--     putStrLn $ show $ map (\ip -> isIpWithSsl ip) $ lines input

-- main = do
--     input <- getContents
--     putStrLn $ show $ map (\ip -> abasFromSequences $ supernetSequences ip) $ lines input

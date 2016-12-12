import Text.Regex.PCRE
import Data.List
import Data.Maybe
import Text.Show.Functions
import qualified Debug.Trace as Trace

type BotOrOutput = (String, Int, [Int])

isInitCommand :: String -> Bool
isInitCommand command = command =~ "value \\d+ goes to bot \\d+"

isActionCommand :: String -> Bool
isActionCommand command = command =~ "bot \\d+ gives low to (bot|output) \\d+ and high to (bot|output) \\d+"

processInitCommand :: [BotOrOutput] -> String -> [BotOrOutput]
processInitCommand bots initCommand = addBotOrOutput "bot" initBotNum initChipNum bots
    where (_, _, _, [chipNumStr, botNumStr]) = initCommand =~ "value (\\d+) goes to bot (\\d+)" :: (String, String, String, [String])
          initChipNum = read chipNumStr :: Int
          initBotNum = read botNumStr :: Int

findBotByNum :: [BotOrOutput] -> Int -> BotOrOutput
findBotByNum bots findingBotNum
    | matchingBots == [] = error $ "No bots with num " ++ (show findingBotNum)
    | otherwise = head $ matchingBots
    where matchingBots = filter (\(t, n ,_) -> t == "bot" && n == findingBotNum) bots

extractChipNumsFromBot :: [BotOrOutput] -> Int -> ([BotOrOutput], [Int])
extractChipNumsFromBot inBots extractBotNum = (outBots, outChipNums)
    where outBots = map (\(t, botNum, chipNums) -> if t == "bot" && botNum == extractBotNum then (t, botNum, []) else (t, botNum, chipNums)) inBots
          (_, _, outChipNums) = findBotByNum inBots extractBotNum

addBotOrOutput :: String -> Int -> Int -> [BotOrOutput] -> [BotOrOutput]
addBotOrOutput addType addNum addChipNum botsOutputs
    | length existingBotsOutputs == 1 = map (\(t, n, cns) -> if t == addType && n == addNum then (t, n, cns ++ [addChipNum]) else (t, n, cns)) botsOutputs
    | otherwise = botsOutputs ++ [(addType, addNum, [addChipNum])]
    where existingBotsOutputs = filter (\(t, n ,_) -> t == addType && n == addNum) botsOutputs

processActionCommand :: [BotOrOutput] -> String -> [BotOrOutput]
processActionCommand inBotsOutputs actionCommand | Trace.trace ("---\n" ++ show inBotsOutputs ++ "\n" ++ actionCommand) False = undefined
processActionCommand inBotsOutputs actionCommand = outBotsOutputs
    where (_, _, _, [sourceBotNumStr, lowTargetType, lowTargetNumStr, highTargetType, highTargetNumStr]) = actionCommand =~ "bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)" :: (String, String, String, [String])
          sourceBotNum = read sourceBotNumStr :: Int
          lowTargetNum = read lowTargetNumStr :: Int
          highTargetNum = read highTargetNumStr :: Int
          (afterExtractBotsOutputs, sourceChipNums) = extractChipNumsFromBot inBotsOutputs sourceBotNum
          lowChipNum = minimum sourceChipNums
          highChipNum = maximum sourceChipNums
          addedHighBotsOutputs = addBotOrOutput highTargetType highTargetNum highChipNum afterExtractBotsOutputs
          outBotsOutputs = addBotOrOutput lowTargetType lowTargetNum lowChipNum addedHighBotsOutputs

processInitCommands :: [String] -> [BotOrOutput]
processInitCommands initCommands = foldl processInitCommand [] initCommands

processActionCommands :: [String] -> [BotOrOutput] -> [BotOrOutput]
processActionCommands [] botsOutputs = botsOutputs
processActionCommands actionCommands botsOutputs = processActionCommands otherActionCommands (processActionCommand botsOutputs nextActionCommand)
    where (_, twoChipsBotNum, _) = head $ filter (\(t, n, chs) -> t == "bot" && length chs == 2) botsOutputs
          nextActionCommand = head $ filter (\c -> c =~ ("bot " ++ show twoChipsBotNum ++ " ")) actionCommands
          otherActionCommands = filter (\c -> c /= nextActionCommand) actionCommands

processCommands :: [String] -> [BotOrOutput]
processCommands commands = processActionCommands actionCommands initBots
    where initCommands = filter isInitCommand commands
          actionCommands = filter isActionCommand commands
          initBots = processInitCommands initCommands

main = do
    input <- getContents
    putStrLn $ show $ processCommands $ lines input

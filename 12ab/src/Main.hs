module Main where

import Text.Regex.PCRE
import qualified Debug.Trace as Trace

type Register = (Char, Int)
type ProgramState = (Int, [Register])

setRegisterByValue :: Char -> Int -> [Register] -> [Register]
setRegisterByValue register value registers = map (\(r, v) -> if r == register then (r, value) else (r, v)) registers

cpyInstruction :: ProgramState -> String -> ProgramState
cpyInstruction (instructionIndex, registers) instruction = (instructionIndex + 1, newRegisters)
    where (_, _, _, [fromStr, toStr]) = instruction =~ "cpy (\\d+|[a-z]) ([a-z])" :: (String, String, String, [String])
          isFromRegister = fromStr =~ "[a-z]" :: Bool
          fromRegister = head fromStr
          fromValue = if isFromRegister then (snd (head (filter (\(r, v) -> r == fromRegister) registers))) else (read fromStr :: Int)
          toRegister = head $ toStr
          newRegisters = setRegisterByValue toRegister fromValue registers

incInstruction :: ProgramState -> String -> ProgramState
incInstruction (instructionIndex, registers) instruction = (instructionIndex + 1, newRegisters)
    where (_, _, _, [registerStr]) = instruction =~ "inc ([a-z])" :: (String, String, String, [String])
          register = head registerStr
          value = (snd (head (filter (\(r, v) -> r == register) registers)))
          newRegisters = setRegisterByValue register (value + 1) registers

decInstruction :: ProgramState -> String -> ProgramState
decInstruction (instructionIndex, registers) instruction = (instructionIndex + 1, newRegisters)
    where (_, _, _, [registerStr]) = instruction =~ "dec ([a-z])" :: (String, String, String, [String])
          register = head registerStr
          value = (snd (head (filter (\(r, v) -> r == register) registers)))
          newRegisters = setRegisterByValue register (value - 1) registers

jnzInstruction :: ProgramState -> String -> ProgramState
jnzInstruction (instructionIndex, registers) instruction = (if checkValue /= 0 then (instructionIndex + distance) else (instructionIndex + 1), registers)
    where (_, _, _, [checkStr, distanceStr]) = instruction =~ "jnz (\\d+|[a-z]) ([-\\d]+)" :: (String, String, String, [String])
          isCheckRegister = checkStr =~ "[a-z]" :: Bool
          checkRegister = head checkStr
          checkValue = if isCheckRegister then (snd (head (filter (\(r, v) -> r == checkRegister) registers))) else (read checkStr :: Int)
          distance = read distanceStr :: Int

runInstruction :: ProgramState -> String -> ProgramState
runInstruction state instruction | Trace.trace (instruction ++ "\t\t" ++ show state) False = undefined
runInstruction state@(instructionIndex, registers) instruction
    | take 3 instruction == "cpy" = cpyInstruction state instruction
    | take 3 instruction == "inc" = incInstruction state instruction
    | take 3 instruction == "dec" = decInstruction state instruction
    | take 3 instruction == "jnz" = jnzInstruction state instruction
    | otherwise = (instructionIndex + 1, registers)

runInstructions :: ProgramState -> [String] -> ProgramState
runInstructions state instructions
    | instructionIndex >= length instructions = state
    | otherwise = runInstructions newState instructions
    where (instructionIndex, registers) = state
          instruction = instructions !! instructionIndex
          newState = runInstruction state instruction

main :: IO ()
main = do
    input <- getContents
    -- Part 1
    -- let initRegisters = [('a', 0), ('b', 0), ('c', 0), ('d', 0)]
    -- Part 2
    let initRegisters = [('a', 0), ('b', 0), ('c', 1), ('d', 0)]
    putStrLn $ show $ runInstructions (0, initRegisters) $ lines input

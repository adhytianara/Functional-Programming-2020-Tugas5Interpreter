module ChurchNumeral where

import Data.Char

convertToChurchNumeral :: [Char] -> [Char]
convertToChurchNumeral input = converterHelper "" input


converterHelper :: [Char] -> [Char] -> [Char]
converterHelper leftOperandNumber (x:xs)
    | x `elem` ['0'..'9'] = converterHelper (leftOperandNumber++[x]) xs 
    | x == '+' = "(λsz." ++ helper (read leftOperandNumber::Int) ++ ")" ++ "(λwyx.y(wyx))" ++ "(λsz." ++ helper (read xs::Int) ++ ")"
    | x == '*' = "(λxyz.x(yz))" ++ "(λsz." ++ helper (read leftOperandNumber::Int) ++ ")" ++ "(λsz." ++ helper (read xs::Int) ++ ")"


helper 0 = "z"
helper input = "s(" ++ helper (input - 1) ++ ")"

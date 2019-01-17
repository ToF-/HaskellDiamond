module Diamond where
import Data.Char

diamond :: Char -> [String]
diamond l = let 
    max = ord l - ord 'A'
    ur  = map letter ['A'..l] 
    letter c = spaces n ++ [c] ++ spaces (max-n)
        where 
        n = ord c - ord 'A'
    spaces x = replicate x ' '
    half = map ((spaces max)++) ur 
  in half ++ drop 1 half

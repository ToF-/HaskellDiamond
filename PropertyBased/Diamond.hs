module Diamond where
import Data.Char

diamond :: Char -> [String]
diamond l = let 
    max = ord l - ord 'A'
    upperLeft  = map letter ['A'..l] 
    letter c = spaces (max-n) ++ [c] ++ spaces n 
        where n = ord c - ord 'A'
    spaces x = replicate x ' '
    upperHalf = map mirror upperLeft
    mirror s = s ++ drop 1 (reverse s)
  in mirror upperHalf

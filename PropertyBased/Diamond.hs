module Diamond where
import Data.Char

diamond :: Char -> [String]
diamond max = mirror half
    where
    half     = map mirror pattern
    mirror s = s ++ drop 1 (reverse s)
    pattern  = map letter ['A'..max]
    letter l = let i = index l in spaces (size - i) ++ [l] ++ spaces i
    index  c = ord c - ord 'A'
    spaces n = replicate n ' '
    size     = ord max - ord 'A'

    

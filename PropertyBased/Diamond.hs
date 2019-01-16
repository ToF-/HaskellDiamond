module Diamond where
import Data.Char

diamond :: Char -> [String]
diamond c = let h = ord c - ord 'A'
                half = [replicate (h*2+1) p | p <- ['A'..c]] 
                half'= drop 1 (reverse half)
            in half ++ half'
    

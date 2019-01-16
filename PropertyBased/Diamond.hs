module Diamond where
import Data.Char

diamond :: Char -> [String]
diamond c = let h = ord c - ord 'A'
    in replicate (h*2+1)  (replicate (h*2+1) 'A')

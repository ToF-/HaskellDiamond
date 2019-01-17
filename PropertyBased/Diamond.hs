module Diamond where
import Data.Char

diamond :: Char -> [String]
diamond l = let size = (ord l - ord '@') * 2 - 1
            in replicate size (replicate size ' ') 

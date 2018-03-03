module Diamond where

diamond 'A' = mirror ["A"]
diamond 'B' = mirror [" A " ,"B B"]
diamond 'C' = mirror ["  A  " ," B B " ,"C   C"] 

mirror xs = xs ++ tail (reverse xs)

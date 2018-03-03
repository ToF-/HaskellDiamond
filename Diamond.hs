module Diamond where

diamond 'A' = mirror (map mirror ["A"])
diamond 'B' = mirror (map mirror [" A" ,"B "])
diamond 'C' = mirror (map mirror ["  A" ," B " ,"C  "])

mirror xs = xs ++ tail (reverse xs)

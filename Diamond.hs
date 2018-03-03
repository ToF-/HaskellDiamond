module Diamond where

diamond c = mirror (map mirror (chain c))

mirror xs = xs ++ tail (reverse xs)

chain 'A' = ["A"]
chain 'B' = [" A","B "]
chain 'C' = ["  A"," B ","C  "]

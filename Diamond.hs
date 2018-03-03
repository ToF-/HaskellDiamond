module Diamond where

diamond 'A' = ["A"]
diamond 'B' = [" A " ,"B B"] ++ [" A "]
diamond 'C' = ["  A  " ," B B " ,"C   C"] ++ tail (reverse ["  A  "," B B ","C   C"])

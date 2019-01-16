import Test.QuickCheck
import Diamond
import Data.List

char :: Gen Char 
char = choose ('A','Z')

quickCheckOf :: String -> Property -> IO ()
quickCheckOf s p = do
    putStr (s ++ "\t")
    quickCheck p

main = do
    quickCheckOf "symmetry" $ forAll char $ 
        \c -> let d = diamond c 
               in d == reverse d && d == map reverse d

    quickCheckOf "starts and ends with a centered A" $ forAll char $
        \c -> let d = diamond c
                  d'= reverse d
                  h = length d `div` 2
              in d!!0!!h == 'A' && d'!!0!!h == 'A'

    quickCheckOf "has the final letter at both ends of the middle line" $ forAll char $
        \c -> let d = diamond c
                  h = length d `div` 2
              in d!!h!!0 == c && (reverse d!!h)!!0 == c 

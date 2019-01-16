import Test.QuickCheck
import Diamond

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
                  h = length d `div` 2
              in d!!0!!h == 'A'

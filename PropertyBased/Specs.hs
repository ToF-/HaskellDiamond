import Test.QuickCheck
import Diamond
import Data.List

char :: Gen Char 
char = choose ('A','Z')

quickCheckOf :: String -> Property -> IO ()
quickCheckOf s p = putStr (s ++ "\t") >> quickCheck p

main = do
    quickCheckOf "symmetry" $ forAll char $ 
        \c -> let d = diamond c 
               in d == reverse d 
               && d == map reverse d

    quickCheckOf "starts and ends with a centered A" $ forAll char $
        \c -> let d = diamond c
                  d'= reverse d
                  h = length d `div` 2
              in d!!0!!h == 'A' && d'!!0!!h == 'A'

    quickCheckOf "has the final letter at both ends of the middle line" $ forAll char $
        \c -> let d = diamond c
                  h = length d `div` 2
              in d!!h!!0 == c && (reverse d!!h)!!0 == c 

    quickCheckOf "position of letter depends on index of letter" $ forAll char $
        \c -> let d = diamond c
                  h = length d `div` 2
                  s = map (drop h) (take (h+1) d)
              in all (\i -> s!!i!!i == ['A'..c]!!i) [0..h] 

    quickCheckOf "any position that is not in the diagonal is a space" $ forAll char $
        \c -> do
            let d = diamond c
            let h = length d `div` 2
            let s = map (drop h) (take (h+1) d)
            i <- choose (0,h)
            j <- choose (0,h)
            return $ (i /= j) == (s!!j!!i == ' ')
                
           

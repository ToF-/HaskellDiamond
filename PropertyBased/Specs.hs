import Test.QuickCheck
import Diamond
import Data.List
import Data.Char

quickCheckOf :: String -> Property -> IO ()
quickCheckOf s p = putStr (s ++ "\t") >> quickCheck p

data Diamond = Diamond { letter :: Char, 
                           size :: Int,   
                          shape :: [String] }
    deriving (Eq, Show)

instance Arbitrary Diamond where
    arbitrary = do
        l <- choose ('A','Z')
        let n = ord l - ord '@' 
        return $ Diamond l (n*2-1) (diamond l)

main = do
    quickCheckOf "has size equal to letter * 2 - 1" 
        $ forAll arbitrary $ 
        \(Diamond c s shape) -> length shape == s

    quickCheckOf "has a diagonal of letters in upper right corner"
        $ forAll arbitrary $ 
        \(Diamond c s shape) -> 
        let h = s `div` 2 
            ur = take (h+1) (map (drop h) shape)
        in all (\(l,i) -> ur!!i!!i == l) (zip ['A'..'Z'] [0..h])
    
    quickCheckOf "has vertical and horizontal simmetry" 
        $ forAll arbitrary $
        \(Diamond c s shape) ->
        let shape' = transpose shape
        in reverse shape == shape && reverse shape' == shape'

            

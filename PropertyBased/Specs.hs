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

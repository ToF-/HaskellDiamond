import Test.Hspec
import Diamond

main = do 
    hspec $ do
        describe "diamond" $ do
            it "when given an 'A' returns a diamond of size 1" $ do
                diamond 'A'  `shouldBe` ["A"]

            it "when given a 'B' returns a diamond of size 3" $ do
                diamond 'B'  `shouldBe` [" A "
                                        ,"B B"
                                        ," A "]
            it "when given a 'C' returns a diamond of size 5" $ do
                diamond 'C'  `shouldBe` ["  A  "
                                        ," B B "
                                        ,"C   C"
                                        ," B B "
                                        ,"  A  "]


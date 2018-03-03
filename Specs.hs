import Test.Hspec
import Diamond

main = hspec $ do
    describe "diamond" $ do
        it "when given an 'A' returns a diamond of size 1" $ do
            diamond 'A'  `shouldBe` ["A"]

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "myParse" $ do
        it "can handle a variable" $ do
            myParse "x" `shouldBe` Right (Variable "x")
        it "can handle an abstraction" $ do
            myParse "λx.x" `shouldBe` Right (Abstraction "x" (Variable "x"))
        it "can handle an application" $ do
            myParse "x x" `shouldBe` Right (Application (Variable "x") (Variable "x"))
    describe "reduce" $ do
        it "applies alpha conversion" $ do
            convergeReduce (Application (Abstraction "x" (Abstraction "y" (Variable "x"))) (Variable "y")) `shouldBe` Right (Abstraction "A" (Variable "y"))
        it "applies beta reduction" $ do
            convergeReduce (Application (Abstraction "x" (Variable "x")) (Variable "x")) `shouldBe` Right (Variable "x")
        it "short circuits variable substitution on tighter scopes" $ do
            convergeReduce (Application (Abstraction "x" (Abstraction "x" (Variable "x"))) (Variable "y")) `shouldBe` Right (Abstraction "A" (Variable "A"))
        it "applies eta reduction" $ do
            convergeReduce (Abstraction "x" (Application (Variable "f") (Variable "x"))) `shouldBe` Right (Variable "f")
    describe "converge" $ do
        it "finds the converging point" $ do
            converge (flip mod 2) 4 `shouldBe` 0
    describe "prettify" $ do
        it "shows a variable" $ do
            prettify (Variable "x") `shouldBe` "x"
        it "shows an abstraction" $ do
            prettify (Abstraction "x" (Variable "x")) `shouldBe` "λx.x"
        it "shows an application" $ do
            prettify (Application (Variable "x") (Variable "x")) `shouldBe` "x x"
        it "uses parenthesis to override associativity" $ do
            prettify (Application (Variable "f") (Application (Variable "f") (Variable "x"))) `shouldBe` "f (f x)"
    describe "execute" $ do
        it "can perform application to an identity combinator" $ do
            execute "(λx.x) y" `shouldBe` "y"

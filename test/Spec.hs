import Test.Tasty (defaultMain)

import Unit.ParseExamples (parseExampleTree)

main :: IO ()
main = defaultMain parseExampleTree

module Days.Day4Spec where

import           Days.Day4
import           Test.Hspec

exampleListNums =
  [ 7
  , 4
  , 9
  , 5
  , 11
  , 17
  , 23
  , 2
  , 0
  , 14
  , 21
  , 24
  , 10
  , 16
  , 13
  , 6
  , 15
  , 25
  , 12
  , 22
  , 18
  , 20
  , 8
  , 19
  , 3
  , 26
  , 1
  ]

exampleTables =
  [ [ [(2, False), (0, False), (12, False), (3, False), (7, False)]
    , [(22, False), (11, False), (13, False), (6, False), (5, False)]
    , [(18, False), (8, False), (23, False), (26, False), (20, False)]
    , [(10, False), (16, False), (15, False), (9, False), (19, False)]
    , [(14, False), (21, False), (17, False), (24, False), (4, False)]
    ]
  , [ [(14, False), (21, False), (16, False), (12, False), (6, False)]
    , [(20, False), (11, False), (10, False), (24, False), (4, False)]
    , [(19, False), (8, False), (7, False), (25, False), (23, False)]
    , [(9, False), (18, False), (13, False), (17, False), (5, False)]
    , [(3, False), (15, False), (0, False), (2, False), (22, False)]
    ]
  , [ [(1, False), (12, False), (20, False), (15, False), (19, False)]
    , [(6, False), (10, False), (3, False), (18, False), (5, False)]
    , [(21, False), (9, False), (14, False), (16, False), (7, False)]
    , [(8, False), (2, False), (23, False), (4, False), (24, False)]
    , [(22, False), (13, False), (17, False), (11, False), (0, False)]
    ]
  ]

exampleAnswerTable =
  [ [(2, True), (0, True), (12, False), (3, False), (7, True)]
  , [(22, False), (11, True), (13, False), (6, False), (5, True)]
  , [(18, False), (8, False), (23, True), (26, False), (20, False)]
  , [(10, False), (16, False), (15, False), (9, True), (19, False)]
  , [(14, True), (21, True), (17, True), (24, True), (4, True)]
  ]

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "readTableLine"
      $          readTableLine "1  2   3    4"
      `shouldBe` [(1, False), (2, False), (3, False), (4, False)]
    it "fileToListAndBingoTables"
      $              fileToListAndBingoTables "./res/day4/example.txt"
      `shouldReturn` (exampleListNums, exampleTables)
    it "isLineComplete" $ do
      isLineComplete [(1, True), (1, True), (1, True), (1, True)]
        `shouldBe` True
      isLineComplete [(1, True), (1, False), (1, True), (1, True)]
        `shouldBe` False
    it "updateTables"
      $ updateTables 2 [[[(2, False), (3, False)], [(4, False), (2, False)]]]
      `shouldBe` [[[(2, True), (3, False)], [(4, False), (2, True)]]]
    it "evaluateTablesAndRunRound"
      $          evaluateTablesAndRunRound 0 exampleListNums exampleTables
      `shouldBe` (24, exampleAnswerTable)
  describe "First Question" $ do
    it "Example input"
      $              firstQuestion "./res/day4/example.txt"
      `shouldReturn` 4512
    it "Result" $ firstQuestion "./res/day4/input.txt" `shouldReturn` 6592
  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day4/example.txt"
      `shouldReturn` 1924
    it "Result" $ secondQuestion "./res/day4/input.txt" `shouldReturn` 31755

main :: IO ()
main = hspec spec

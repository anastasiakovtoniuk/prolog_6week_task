-- Малій Олександра Михайлівна
-- Malij Oleksandra Mykhailivna
-- File: Test suite for the Haskell BST

module Main where

import Bst
import System.Exit (exitFailure)

-- =========================================================================
-- Data types describing the structured test output
-- =========================================================================

data TestCaseResult = TestCaseResult
    { testLabel  :: String
    , testPassed :: Bool
    }

data TestSection = TestSection
    { sectionLabel :: String
    , sectionCases :: [TestCaseResult]
    }

testCase :: Equatable a => Int -> String -> a -> a -> TestCaseResult
testCase n name expected actual =
    TestCaseResult (formatLabel n name) (actual == expected)

testCaseBool :: Int -> String -> Bool -> TestCaseResult
testCaseBool n name condition =
    TestCaseResult (formatLabel n name) condition

formatLabel :: Int -> String -> String
formatLabel n name = "Test " ++ show n ++ ": " ++ name

type Equatable a = Eq a

-- =========================================================================
-- Sections definition
-- =========================================================================

sections :: [TestSection]
sections =
    [ TestSection "Testing basic operations" basicCases
    , TestSection "Testing set operations" setCases
    , TestSection "Testing tree properties" treePropertyCases
    , TestSection "Testing BST validation" validationCases
    , TestSection "Testing tree traversals" traversalCases
    ]

basicCases :: [TestCaseResult]
basicCases =
    [ testCaseBool 1 "Creating an empty tree" (isEmpty emptyTree)
    , testCase 2 "Inserting a single element" (Node 5 Empty Empty) (insert 5 Empty)
    , testCase 3 "Inserting multiple elements" [1, 3, 5, 7, 9] (toList $ fromList [5, 3, 7, 1, 9])
    , testCaseBool 4 "Searching for an existing element" (search 7 $ fromList [5, 3, 7, 1, 9])
    , testCaseBool 5 "Searching for a missing element" (not $ search 10 $ fromList [5, 3, 7, 1, 9])
    , testCase 6 "Deleting a leaf node" [3, 5, 7, 9] (toList $ delete 1 $ fromList [5, 3, 7, 1, 9])
    , testCase 7 "Deleting a node with one child" [3, 5, 9] (toList $ delete 7 $ fromList [5, 3, 7, 9])
    , testCase 8 "Deleting a node with two children" [1, 3, 4, 6, 7, 9] (toList $ delete 5 $ fromList [5, 3, 7, 1, 4, 6, 9])
    ]

setCases :: [TestCaseResult]
setCases =
    [ testCase 9 "Union of two trees" [1, 3, 5, 7, 9, 11] (toList $ unionBST tree1 tree2)
    , testCase 10 "Intersection of two trees" [5, 7] (toList $ intersectionBST tree1 tree2)
    , testCase 11 "Difference of two trees" [1, 3] (toList $ differenceBST tree1 tree2)
    , testCase 12 "Symmetric difference" [1, 3, 9, 11] (toList $ symmetricDifferenceBST tree1 tree2)
    ]
  where
    tree1 = fromList [1, 3, 5, 7]
    tree2 = fromList [5, 7, 9, 11]

treePropertyCases :: [TestCaseResult]
treePropertyCases =
    [ testCase 13 "Tree size" 5 (size tree)
    , testCase 14 "Tree height" 3 (height tree)
    , testCase 15 "Minimum value" 1 (findMin tree)
    , testCase 16 "Maximum value" 9 (findMax tree)
    ]
  where
    tree = fromList [5, 3, 7, 1, 9]

validationCases :: [TestCaseResult]
validationCases =
    [ testCaseBool 17 "Validating a correct tree" (isValidBST validTree)
    , testCaseBool 18 "Detecting an incorrect tree" (not $ isValidBST invalidTree)
    , testCaseBool 19 "Validating an empty tree" (isValidBST (Empty :: Tree Int))
    ]
  where
    validTree = fromList [5, 3, 7, 1, 9]
    invalidTree = Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty)

traversalCases :: [TestCaseResult]
traversalCases =
    [ testCase 20 "Inorder traversal" [1, 3, 4, 5, 6, 7, 9] (inorder tree)
    , testCase 21 "Preorder traversal" [5, 3, 1, 4, 7, 6, 9] (preorder tree)
    , testCase 22 "Postorder traversal" [1, 4, 3, 6, 9, 7, 5] (postorder tree)
    ]
  where
    tree = fromList [5, 3, 7, 1, 4, 6, 9]

-- =========================================================================
-- Reporting utilities
-- =========================================================================

reportSection :: TestSection -> IO [TestCaseResult]
reportSection TestSection{sectionLabel = label, sectionCases = cases} = do
    putStrLn $ "\n=== " ++ label ++ " ==="
    mapM_ (putStrLn . formatCase) cases
    pure (filter (not . testPassed) cases)
  where
    formatCase c = testLabel c ++ "... " ++ status c
    status c
        | testPassed c = "PASSED"
        | otherwise    = "FAILED"

totalTests :: Int
totalTests = sum (map (length . sectionCases) sections)

runAllTests :: IO ()
runAllTests = do
    putStrLn "Starting the Haskell BST test suite..."
    failed <- concat <$> mapM reportSection sections
    let passed = totalTests - length failed
    putStrLn $ "\nSummary: " ++ show passed ++ "/" ++ show totalTests ++ " tests passed."
    if null failed
        then putStrLn "\nAll tests finished!"
        else do
            putStrLn "\nFailed tests:"
            mapM_ (putStrLn . (" - " ++) . testLabel) failed
            exitFailure

-- =========================================================================
-- Main entry point
-- =========================================================================

main :: IO ()
main = runAllTests

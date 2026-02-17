-- Малій Олександра Михайлівна
-- Модуль: Допоміжні функції для BST

module Bst.Utils
    ( fromList
    , toList
    , inorder
    , preorder
    , postorder
    , levelorder
    , printTree
    , showTree
    , isValidBST
    ) where

import Bst.Basic
import Data.Foldable (foldl')

-- ============================================================================
-- Конверсія між деревом та списком
-- ============================================================================

fromList :: Ord a => [a] -> Tree a
fromList = foldl' (flip insert) Empty

toList :: Tree a -> [a]
toList = inorder

-- ============================================================================
-- Обходи дерева
-- ============================================================================

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node val left right) = [val] ++ preorder left ++ preorder right

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node val left right) = postorder left ++ postorder right ++ [val]

levelorder :: Tree a -> [a]
levelorder tree = levelorderHelper [tree]
    where
        levelorderHelper [] = []
        levelorderHelper trees = 
            [val | Node val _ _ <- trees] ++ 
            levelorderHelper [child | Node _ left right <- trees, child <- [left, right], not (isEmpty child)]

-- ============================================================================
-- Візуалізація дерева
-- ============================================================================

printTree :: Show a => Tree a -> IO ()
printTree tree = putStrLn $ unlines $ printTreeHelper tree

printTreeHelper :: Show a => Tree a -> [String]
printTreeHelper Empty = []
printTreeHelper (Node val left right) =
    [show val] ++ 
    indent "├── " "│   " (printTreeHelper left) ++
    indent "└── " "    " (printTreeHelper right)
    where
        indent first rest = zipWith (++) (first : repeat rest)

showTree :: Show a => Tree a -> String
showTree = unlines . showTreeHelper 0
    where
        showTreeHelper _ Empty = []
        showTreeHelper indent (Node val left right) =
            showTreeHelper (indent + 4) right ++
            [replicate indent ' ' ++ show val] ++
            showTreeHelper (indent + 4) left

-- ============================================================================
-- Валідація BST
-- ============================================================================

isValidBST :: Ord a => Tree a -> Bool
isValidBST tree = isValidBSTHelper tree Nothing Nothing

isValidBSTHelper :: Ord a => Tree a -> Maybe a -> Maybe a -> Bool
isValidBSTHelper Empty _ _ = True
isValidBSTHelper (Node val left right) minVal maxVal =
    checkMin && checkMax && isValidBSTHelper left minVal (Just val) 
                         && isValidBSTHelper right (Just val) maxVal
    where
        checkMin = case minVal of
            Nothing -> True
            Just m  -> val > m
        checkMax = case maxVal of
            Nothing -> True
            Just m  -> val < m

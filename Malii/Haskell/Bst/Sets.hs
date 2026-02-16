-- Малій Олександра Михайлівна
-- Модуль: Операції над множинами для BST

module Bst.Sets
    ( unionBST
    , intersectionBST
    , differenceBST
    , symmetricDifferenceBST
    ) where

import Bst.Basic
import Bst.Utils
import Data.List (union, intersect, (\\))

-- ============================================================================
-- Операції над множинами
-- ============================================================================

unionBST :: Ord a => Tree a -> Tree a -> Tree a
unionBST tree1 tree2 = fromList (toList tree1 `union` toList tree2)

intersectionBST :: Ord a => Tree a -> Tree a -> Tree a
intersectionBST tree1 tree2 = fromList (toList tree1 `intersect` toList tree2)

differenceBST :: Ord a => Tree a -> Tree a -> Tree a
differenceBST tree1 tree2 = fromList (toList tree1 \\ toList tree2)

symmetricDifferenceBST :: Ord a => Tree a -> Tree a -> Tree a
symmetricDifferenceBST tree1 tree2 =
    fromList ((toList tree1 \\ toList tree2) ++ (toList tree2 \\ toList tree1))

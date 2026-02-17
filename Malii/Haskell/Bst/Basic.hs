-- Малій Олександра Михайлівна
{-# LANGUAGE InstanceSigs #-}

-- Модуль: Базові операції з бінарного дерева пошуку

module Bst.Basic
    ( Tree(..)
    , emptyTree
    , isEmpty
    , search
    , member
    , insert
    , delete
    , findMin
    , findMax
    , size
    , height
    ) where

-- ============================================================================
-- Тип даних бінарного дерева пошуку
-- ============================================================================

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- ============================================================================
-- Створення та перевірка порожнього дерева
-- ============================================================================

emptyTree :: Tree a
emptyTree = Empty

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- ============================================================================
-- Операція пошуку
-- ============================================================================

search :: Ord a => a -> Tree a -> Bool
search _ Empty = False
search x (Node val left right)
    | x == val  = True
    | x < val   = search x left
    | otherwise = search x right

member :: Ord a => a -> Tree a -> Bool
member = search

-- ============================================================================
-- Операція вставки
-- ============================================================================

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node val left right)
    | x == val  = Node val left right
    | x < val   = Node val (insert x left) right
    | otherwise = Node val left (insert x right)

-- ============================================================================
-- Операція видалення
-- ============================================================================

delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty = Empty
delete x (Node val left right)
    | x < val   = Node val (delete x left) right
    | x > val   = Node val left (delete x right)
    | otherwise = deleteRoot left right

-- ---------------------------------------------------------------------------
-- deleteRoot(++Left, ++Right, --Tree)
deleteRoot :: Ord a => Tree a -> Tree a -> Tree a
deleteRoot Empty right = right
deleteRoot left Empty  = left
deleteRoot left right  = Node minVal left (delete minVal right)
    where minVal = findMin right

-- ============================================================================
-- Пошук мінімального та максимального значення
-- ============================================================================

findMin :: Tree a -> a
findMin (Node val Empty _) = val
findMin (Node _ left _)    = findMin left
findMin Empty              = error "findMin: empty tree"

findMax :: Tree a -> a
findMax (Node val _ Empty) = val
findMax (Node _ _ right)   = findMax right
findMax Empty              = error "findMax: empty tree"

-- ============================================================================
-- Властивості дерева
-- ============================================================================

size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

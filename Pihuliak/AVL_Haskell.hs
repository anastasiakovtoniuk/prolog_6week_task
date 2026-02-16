import Data.List (union, intersect, (\\), sort, nub)

-- Структура вузла: Node Значення Висота Ліве_дерево Праве_дерево
data AVL a = Empty 
           | Node a Int (AVL a) (AVL a)
           deriving (Show, Eq)

-- -----------------------------------------------------------------
-- height :: AVL a -> Int
-- Отримує висоту вузла.
-- -----------------------------------------------------------------
height :: AVL a -> Int
height Empty = 0
height (Node _ h _ _) = h

-- -----------------------------------------------------------------
-- node :: a -> AVL a -> AVL a -> AVL a
-- Розумний конструктор, який автоматично обчислює висоту.
-- -----------------------------------------------------------------
node :: a -> AVL a -> AVL a -> AVL a
node v l r = Node v (max (height l) (height r) + 1) l r

-- -----------------------------------------------------------------
-- balanceFactor :: AVL a -> Int
-- Обчислює різницю висот (BF).
-- -----------------------------------------------------------------
balanceFactor :: AVL a -> Int
balanceFactor Empty = 0
balanceFactor (Node _ _ l r) = height l - height r

-- =================================================================
-- РОТАЦІЇ
-- =================================================================

rotateRight :: AVL a -> AVL a
rotateRight (Node y _ (Node x _ l mid) r) = node x l (node y mid r)
rotateRight t = t

rotateLeft :: AVL a -> AVL a
rotateLeft (Node x _ l (Node y _ mid r)) = node y (node x l mid) r
rotateLeft t = t

-- -----------------------------------------------------------------
-- balance :: AVL a -> AVL a
-- Відновлює властивості АВЛ-дерева.
-- -----------------------------------------------------------------
balance :: AVL a -> AVL a
balance t@(Node v h l r)
    | balanceFactor t > 1  = if balanceFactor l >= 0 
                             then rotateRight t
                             else rotateRight (node v (rotateLeft l) r)
    | balanceFactor t < -1 = if balanceFactor r <= 0 
                             then rotateLeft t
                             else rotateLeft (node v l (rotateRight r))
    | otherwise = t
balance Empty = Empty

-- =================================================================
-- ОСНОВНІ ОПЕРАЦІЇ
-- =================================================================

-- -----------------------------------------------------------------
-- insert :: (Ord a) => a -> AVL a -> AVL a
-- Вставка з балансуванням.
-- -----------------------------------------------------------------
insert :: (Ord a) => a -> AVL a -> AVL a
insert x Empty = node x Empty Empty
insert x t@(Node v h l r)
    | x < v     = balance (node v (insert x l) r)
    | x > v     = balance (node v l (insert x r))
    | otherwise = t 

-- -----------------------------------------------------------------
-- treeToList :: AVL a -> [a]
-- In-order обхід (повертає відсортований список).
-- -----------------------------------------------------------------
treeToList :: AVL a -> [a]
treeToList Empty = []
treeToList (Node v _ l r) = treeToList l ++ [v] ++ treeToList r

-- -----------------------------------------------------------------
-- buildTree :: (Ord a) => [a] -> AVL a
-- Побудова дерева. Використовуємо nub . sort для чистих множин.
-- -----------------------------------------------------------------
buildTree :: (Ord a) => [a] -> AVL a
buildTree xs = foldl (flip insert) Empty (nub . sort $ xs)

-- =================================================================
-- ОПЕРАЦІЇ НАД МНОЖИНАМИ
-- =================================================================

-- -----------------------------------------------------------------
-- avlUnion, avlIntersect, avlDiff
-- Реалізація через списки: Tree -> List -> Op -> Tree.
-- -----------------------------------------------------------------
avlUnion :: (Ord a) => AVL a -> AVL a -> AVL a
avlUnion t1 t2 = buildTree (union (treeToList t1) (treeToList t2))

avlIntersect :: (Ord a) => AVL a -> AVL a -> AVL a
avlIntersect t1 t2 = buildTree (intersect (treeToList t1) (treeToList t2))

avlDiff :: (Ord a) => AVL a -> AVL a -> AVL a
avlDiff t1 t2 = buildTree (treeToList t1 \\ treeToList t2)

-- =================================================================
-- ТЕСТИ
-- =================================================================

runTests :: IO ()
runTests = do
    let s1 = [1, 2, 3, 4, 5]
    let s2 = [4, 5, 6, 7, 8]
    let t1 = buildTree s1
    let t2 = buildTree s2

    putStrLn "--- Running AVL Set Operations Tests ---"
    
    -- Тест Union
    let resUnion = treeToList (avlUnion t1 t2)
    printTest "Union" [1,2,3,4,5,6,7,8] resUnion

    -- Тест Intersection
    let resInter = treeToList (avlIntersect t1 t2)
    printTest "Intersection" [4,5] resInter

    -- Тест Difference
    let resDiff = treeToList (avlDiff t1 t2)
    printTest "Difference (t1 - t2)" [1,2,3] resDiff

    putStrLn "--- Tests Completed ---"

printTest :: (Eq a, Show a) => String -> a -> a -> IO ()
printTest name expected actual =
    if expected == actual
    then putStrLn $ "[OK] " ++ name
    else putStrLn $ "[FAIL] " ++ name ++ " | Expected: " ++ show expected ++ " Got: " ++ show actual

-- -----------------------------------------------------------------
-- printTree :: (Show a) => AVL a -> IO ()
-- Візуалізація.
-- -----------------------------------------------------------------
printTree :: (Show a) => AVL a -> IO ()
printTree t = putStr (draw t 0)
  where
    draw Empty _ = ""
    draw (Node v _ l r) indent =
        draw r (indent + 6) ++
        replicate indent ' ' ++ show v ++ "\n" ++
        draw l (indent + 6)

-- =========================================================================
-- Main entry point
-- =========================================================================

main :: IO ()
main = runAllTests

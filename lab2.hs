-- First Task
func :: (Ord a, Num a) => [a] -> [a] -> [a]
func xx@(x : _) yy@(y : _) | x * y > 0 = [x, y]
                           | otherwise = last xx : tail yy
func _ _ = []

-- Secong Task
data Tree23 a = Empty
              | Leaf  a
              | Node2 a   (Tree23 a) (Tree23 a)
              | Node3 a a (Tree23 a) (Tree23 a) (Tree23 a)
    deriving (Show)

showTree23 :: Show a => Tree23 a -> IO()
showTree23 = showTree23' 0

showTree23' :: Show a => Int -> Tree23 a -> IO()
showTree23' h Empty             = putStrLn $ indent h ++ "Empty"
showTree23' h (Leaf a)          = putStrLn $ indent h ++ "Leaf " ++ show a
showTree23' h (Node2 x l r)     = do putStrLn $ indent h ++ "Node2 " ++ show x
                                     showTree23' (h + 1) l
                                     showTree23' (h + 1) r
showTree23' h (Node3 x y l m r) = do putStrLn $ indent h ++ "Node3 " ++ show x ++ " " ++ show y
                                     showTree23' (h + 1) l
                                     showTree23' (h + 1) m
                                     showTree23' (h + 1) r

indent :: Int -> String
indent 0 = ""
indent h = "    " ++ indent (h - 1)

main :: IO ()
main = do
    putStrLn "func [2,4,6] [1,3,5]"
    print $ func [2, 4, 6] [1, 3, 5]

    putStrLn "func [-2,4,6] [1,3,5]"
    print $ func [-2, 4, 6] [1, 3, 5]

    putStrLn "func [-9] [9]"
    print $ func [-9] [9]


    let tree = Node2 2
                (Node2 7
                    (Node2 2 (Leaf 8) (Leaf 9))
                    (Node2 6 (Leaf 5) (Leaf 4)))
                (Node2 3
                    (Node2 17 (Leaf 14) (Leaf 1))
                    (Node2 10 (Leaf 14) (Leaf 11)))

    putStrLn "Tree example:"
    showTree23 tree
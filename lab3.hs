{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
data Tree23 a = Empty
              | Leaf  a
              | Node2 a (Tree23 a) (Tree23 a)
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

step1 :: [a] -> Tree23 a
step1 a = Leaf $ a!!0

step2 :: [a] -> Tree23 a
step2 a = Node3 (a!!0) (a!!1)
            Empty Empty Empty

step3 :: [a] -> Tree23 a
step3 a = Node2 (a!!1)         -- root
            (Leaf $ a!!0)      -- left branch
            (Leaf $ a!!2)      -- right branch

step4 :: [a] -> Tree23 a
step4 a = Node2 (a!!1)                               -- root
            (Leaf $ a!!0)                            -- left branch
            (Node3 (a!!2) (a!!3) Empty Empty Empty)  -- right branch

step5 :: [a] -> Tree23 a
step5 a = Node3 (a!!1) (a!!3)     -- root
            (Leaf $ a!!0)         -- left branch
            (Leaf $ a!!2)         -- middle branch
            (Leaf $ a!!4)         -- right branch

step6 :: [a] -> Tree23 a
step6 a = Node3 (a!!1) (a!!3)       -- root
            (Leaf $ a!!0)           -- left branch
            (Leaf $ a!!2)           -- middle branch
            (Node3 (a!!4) (a!!5)    -- right branch
                Empty Empty Empty)

step7 :: [a] -> Tree23 a
step7 a = Node2 (a!!3)              -- root
            (Node2 (a!!1)           -- left branch
                (Leaf $ a!!0)
                (Leaf $ a!! 2))
            (Node3 (a!!5) (a!!7)    -- right branch
                (Leaf $ a!!4)
                (Leaf $ a!!6)
                (Leaf $ a!!8))

main :: IO ()
main = do
    let list = [1..9]
    putStrLn "Step 1:"
    showTree23 $ step1 list
    putStrLn ""

    putStrLn "Step 2:"
    showTree23 $ step2 list
    putStrLn ""

    putStrLn "Step 3:"
    showTree23 $ step3 list
    putStrLn ""

    putStrLn "Step 4:"
    showTree23 $ step4 list
    putStrLn ""

    putStrLn "Step 5:"
    showTree23 $ step5 list
    putStrLn ""

    putStrLn "Step 6:"
    showTree23 $ step6 list
    putStrLn ""

    putStrLn "Step 7:"
    showTree23 $ step7 list
    putStrLn ""

    putStrLn "Tree visualization:"
    putStrLn "       4        "
    putStrLn "    /     \\    "
    putStrLn "   2      6 8   "
    putStrLn "  / \\    / | \\"
    putStrLn " 1   3  5  7  9 "
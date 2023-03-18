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


instance Functor Tree23 where
    fmap _ Empty             = Empty
    fmap f (Leaf a)          = Leaf  (f a)
    fmap f (Node2 x l r)     = Node2 (f x) (fmap f l) (fmap f r)
    fmap f (Node3 x y l m r) = Node3 (f x) (f y) (fmap f l) (fmap f m) (fmap f r)

main :: IO ()
main = do
    let tree = Node2 2
                (Node2 7
                    (Node2 2 (Leaf 8) (Leaf 9))
                    (Node2 6 (Leaf 5) (Leaf 4)))
                (Node2 3
                    (Node2 17 (Leaf 14) (Leaf 1))
                    (Node2 10 (Leaf 14) (Leaf 11)))

    let f = (^2)

    putStrLn  "Tree example:"
    showTree23 tree

    putStrLn "Tree after use function (^2):"
    showTree23 $ fmap f tree
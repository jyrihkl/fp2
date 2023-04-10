{-import Control.Applicative -- this line may be unnecessary in your local testing if you have a more recent version of the Haskell platform

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Read, Eq)

makeFunc ('+',x) = (+x)
makeFunc ('*',x) = (*x)
makeFunc _ = \x -> 0

main = do
    input1 <- getLine
    input2 <- getLine
    print $ (fmap makeFunc (read input1 :: (BinaryTree (Char,Int)) ) ) <*> (read input2:: (BinaryTree Int))

-- Your code starts here-}

instance Functor BinaryTree where
    fmap f Empty = Empty
    fmap f (Node x y z) = Node (f x) (fmap f y) (fmap f z)

instance Applicative BinaryTree where
    pure x = Node x Empty Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Node f l1 r1 <*> Node x l2 r2 = Node (f x) (l1 <*> l2) (r1 <*> r2)
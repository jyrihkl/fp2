{-import Data.Monoid
import Data.Foldable

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Read, Eq)

-- for Or we require one True, so for us (evalOr Empty) gives False
evalOr :: (BinaryTree Any) -> Any
evalOr tree = Data.Foldable.foldl (mappend) (Any False) tree


main = do
    input1 <- getLine
    input2 <- getLine
    print $ getAny $ existsInTree (read input1 :: (BinaryTree Int)) (>(read input2 :: Int))

-}
existsInTree :: (BinaryTree a) -> (a -> Bool) -> Any
existsInTree tree f = evalOr $ fmap (\n -> Any (f n)) tree

instance Functor BinaryTree where
    fmap f Empty = Empty
    fmap f (Node x y z) = Node (f x) (fmap f y) (fmap f z)

instance Foldable BinaryTree where
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = foldMap f l `mappend`  
                             f x         `mappend`  
                             foldMap f r
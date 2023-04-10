-- existsInTree :: (BinaryTree a) -> (a -> Bool) -> Bool
existsInTree tree value = evalOr $ fmap value tree

instance Functor BinaryTree where
    fmap f Empty = Empty
    fmap f (Node x y z) = Node (f x) (fmap f y) (fmap f z)
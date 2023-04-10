import Control.Applicative -- this line may be unnecessary in your local testing if you have a more recent version of the Haskell platform

makeFuncList :: [(Char,Int)] -> [(Int->Int)]
makeFuncList xs = map makeFunc xs

makeFunc ('+',x) = (+x)
makeFunc ('*',x) = (*x)
makeFunc _ = \x -> 0


main = do
    input1 <- getLine
    input2 <- getLine
    print $ getRevZipList ( (RevZipList (makeFuncList (read input1 :: [(Char,Int)]) ) ) <*> (RevZipList (read input2 :: [Int])) ) 

newtype RevZipList a = RevZipList { getRevZipList :: [a] }

-- Your code starts here

instance Functor RevZipList where
    fmap f (RevZipList rzl) = RevZipList (fmap f $ reverse rzl)

instance Applicative RevZipList where
    pure x = RevZipList (repeat x)
    RevZipList fs <*> RevZipList xs = RevZipList $ reverse $ zipWith (\f x -> f x) (reverse fs) (reverse xs)
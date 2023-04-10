-- MaybeNull:
{-
data MaybeNull a = JustVal a | Null deriving (Show, Read)

-- Bool3:

data Bool3 = False3 | Unk3 | True3 deriving (Eq,Show,Read) 

(&&&) :: Bool3 -> Bool3 -> Bool3
(&&&) x y
    | x == True3 && y == True3 = True3
    | x == False3 || y == False3 = False3
    |otherwise = Unk3

(|||) :: Bool3 -> Bool3 -> Bool3
(|||) x y
    | x == True3 || y == True3 = True3
    | x == False3 && y == False3 = False3
    |otherwise = Unk3

not3 :: Bool3 -> Bool3
not3 x
    | x == True3 = False3
    | x == False3 = True3
    |otherwise = Unk3

main = do
    input1 <- getLine
    input2 <- getLine
    print $ applyToMaybeNull (read input1::Bool3) (read input2 :: [(MaybeNull Bool3)] )

applyToMaybeNull :: Bool3 -> [(MaybeNull Bool3)] -> [(MaybeNull Bool3)] 
applyToMaybeNull y [] = []
applyToMaybeNull y (x:xs)
    = (x >>= (\x -> return ((&&&) x y))) :
    applyToMaybeNull y xs
-}
instance Functor MaybeNull where
    fmap f Null = Null
    fmap f (JustVal a) = JustVal (f a)

instance Applicative MaybeNull where
    pure = JustVal
    Null <*> mn = Null
    JustVal f <*> mn = fmap f mn

instance Monad MaybeNull where
    return = JustVal
    Null >>= f = Null
    JustVal x >>= f = f x
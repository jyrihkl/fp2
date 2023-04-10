{-import Data.Monoid

-- MaybeNull:

data MaybeNull a = JustVal a | Null deriving (Show, Read)

-- Bool3:

data Bool3 = False3 | Unk3 | True3 deriving (Eq,Show,Read) 

(&&&) :: Bool3 -> Bool3 -> Bool3
(&&&) x y
    | x == True3 && y == True3 = True3
    | x == False3 || y == False3 = False3
    | otherwise = Unk3

(|||) :: Bool3 -> Bool3 -> Bool3
(|||) x y
    | x == True3 || y == True3 = True3
    | x == False3 && y == False3 = False3
    | otherwise = Unk3

not3 :: Bool3 -> Bool3
not3 x
    | x == True3 = False3
    | x == False3 = True3
    | otherwise = Unk3

main = do
    input1 <- getLine
    print (mappendMaybeNullBool3s
    (read input1 :: [(MaybeNull Bool3)] ))

mappendMaybeNullBool3s bs = foldl (\acc b -> mappend acc b) mempty bs-}

-- Your code starts here:

instance Monoid Bool3 where
    mempty = True3
    b1 `mappend` b2 = b1 &&& b2

instance Monoid a => Monoid (MaybeNull a) where
    mempty = Null
    m `mappend` Null = Null
    Null `mappend` m = Null
    JustVal m1 `mappend` JustVal m2 = JustVal (m1 `mappend` m2)

-- MaybeNull:

{-data MaybeNull a = JustVal a | Null deriving (Show, Read)

--

makeFunc :: (Char,Int) -> (Int -> (MaybeNull Int))
makeFunc ('+',x) = \y -> JustVal (x+y) 
makeFunc ('*',x) = \y -> JustVal (x*y)
makeFunc _ = \x -> Null

main = do
    input1 <- getLine
    print $ myFunc (read input1 :: [(Char,Int)]) (JustVal 1)

myFunc :: [(Char,Int)] -> (MaybeNull Int) -> (MaybeNull Int) -}
myFunc pairs mn = foldl (\acc pair -> applyMaybeNull acc $ makeFunc pair) mn pairs

applyMaybeNull :: MaybeNull a -> (a -> MaybeNull b) -> MaybeNull b
applyMaybeNull Null f = Null
applyMaybeNull (JustVal x) f = f x
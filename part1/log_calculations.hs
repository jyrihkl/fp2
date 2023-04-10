{-import Data.Monoid

makeFunc ('+',x) = (+x)
makeFunc ('*',x) = (*x)
makeFunc _ = \x -> 0

main = do
    input1 <- getLine
    print $ myFunc (read input1 :: [(Char,Int)]) (1,"")

applyLog :: Monoid b => (a,b) -> (a -> (a,b)) -> (a,b) 
applyLog (x,log) f = let (y,logEntry) = f x in (y,log `mappend` logEntry) 

myFunc :: [(Char,Int)] -> (Int,String) -> (Int,String) -}
myFunc l s = foldl (\acc p -> applyLog acc (makeEntry p)) s l

makeEntry :: (Char, Int) -> (Int -> (Int, String))
makeEntry (c, i) =
    let f = makeFunc (c, i)
    in (\n -> (f n, "Applied function " ++ (show c) ++ (show i) ++ ";"))

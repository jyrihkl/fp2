{-import Data.List
import Control.Monad

sortTriangle (a,b,c) = (x,y,z)
  where x:y:[z] = sort [a,b,c]

main = do
 input1 <- getLine
 print $ sort $ map sortTriangle $ rightTriangles (read input1 :: Int)

rightTriangles :: Int -> [(Int,Int,Int)]
rightTriangles length = do
   c <- [1..length] -}
   b <- [1..c]
   a <- [1..b]
   guard (a+b+c == length && a^2 + b^2 == c^2)
   return (a, b, c)
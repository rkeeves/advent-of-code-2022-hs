import           Control.Arrow ((>>>))
import           Data.List     (sortBy)

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> splitBy (== "") >>> fmap (fmap read >>> sum) >>> descending >>> take 1 >>> sum >>> show

b :: String -> String
b = lines >>> splitBy (== "") >>> fmap (fmap read >>> sum) >>> descending >>> take 3 >>> sum >>> show

descending :: [Int] -> [Int]
descending = sortBy (flip compare)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = foldr (\x (xs : xss) -> if f x then [] : (xs : xss) else (x : xs) : xss) [[]]

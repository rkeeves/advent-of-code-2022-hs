import           Control.Arrow ((>>>))
import           Data.List     (nub, tails)

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [Char] -> String
a = markerOfLength 4 >>> show

b :: [Char] -> String
b = markerOfLength 14 >>> show

markerOfLength :: Eq a => Int -> [a] -> Int
markerOfLength n = tails >>> fmap (take n >>> nub >>> length) >>> zip [n..] >>> filter (snd >>> (>= n)) >>> head >>> fst

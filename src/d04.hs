import           Control.Arrow ((>>>))

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> fmap rangePair >>> filter (eitherway (\(a, b) (c, d) -> c <= a && b <= d)) >>> length >>> show

b :: String -> String
b = lines >>> fmap rangePair >>> filter (eitherway (\(a, b) (c, d) -> not (b < c || d < a))) >>> length >>> show

type Range = (Int, Int)

eitherway :: (a -> a -> Bool) -> (a, a) -> Bool
eitherway rel (x, y) = x `rel` y || y `rel` x

rangePair :: String -> (Range, Range)
rangePair =  fmap (\x -> if x `elem` ",-" then ' ' else x) >>> words >>> fmap read >>> (\[a, b, c, d] -> ((a, b), (c, d)))

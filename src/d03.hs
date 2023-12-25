import           Control.Arrow   ((>>>))
import           Data.List       (unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> fmap (halves >>> toList >>> intersect' >>> head >>> scoreOf) >>> sum >>> show

b :: String -> String
b = lines >>> chunksOf 3 >>> fmap (intersect' >>> head >>> scoreOf) >>> sum >>> show

halves :: [a] -> ([a], [a])
halves xs = splitAt ((length xs + 1)  `div` 2) xs

toList :: (a, a) -> [a]
toList (x, y) = [x, y]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if n < 0 || null xs then Nothing else Just (splitAt n xs))

intersect' :: Eq a => [[a]] -> [a]
intersect' (x:xs) = filter (\c -> all (c `elem`) xs) x

scoreOf :: Char -> Int
scoreOf key = Map.findWithDefault 0 key scores

scores :: Map Char Int
scores = Map.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1..]

import           Control.Arrow ((>>>))
import           Data.List     (tails, transpose)

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = readMatrix >>> fourways >>> maprows (withTail visible) >>> unfourways >>> matrixZipWith (||) >>> cells >>> countTrues >>> show
    where
        countTrues = length . filter id

b :: String -> String
b = readMatrix >>> fourways >>> maprows (withTail scenicScore) >>> unfourways >>> matrixZipWith (*) >>> cells >>> maximum >>> show

type Row a      = [a]
type Matrix a   = [Row a]
type FourWays a = (Matrix a, Matrix a, Matrix a, Matrix a)

cells :: Matrix a -> [a]
cells = concat

withTail :: (a -> [a] -> b) -> Row a -> Row b
withTail f as = zipWith f as (drop 1 . tails $ as)

visible :: Ord a => a -> [a] -> Bool
visible x = all (< x)

scenicScore :: Ord a => a -> [a] -> Int
scenicScore x xs = let (ys, ys') = span (< x) xs in length ys + if null ys' then 0 else 1

matrixZipWith :: (a -> a -> a) -> [Matrix a] -> Matrix a
matrixZipWith f (m : ms) = let (<+>) = zipWith (zipWith f) in foldr (<+>) m ms

unfourways :: FourWays a -> [Matrix a]
unfourways (as, bs, cs, ds) = [as, fmap reverse bs, transpose cs, (fmap reverse >>> transpose) ds]

maprows :: (Row a -> Row b) -> FourWays a -> FourWays b
maprows f (as, bs, cs, ds) = (f <$> as, f <$> bs, f <$> cs, f <$> ds)

fourways :: Matrix a -> FourWays a
fourways m = (m, fmap reverse m, transpose m, transpose >>> fmap reverse $ m)

readMatrix :: String -> Matrix Char
readMatrix = lines

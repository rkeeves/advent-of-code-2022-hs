import           Control.Arrow ((>>>))
import           Data.List     (foldl', unfoldr)

main :: IO ()
main = interact $ lines >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [String] -> String
a = fmap fromSnafu >>> sum >>> toSnafu

b :: [String] -> String
b _ = "There's no part B puzzle."

toSnafu :: Int -> String
toSnafu = Just >>> unfoldr toSnafu' >>> reverse

toSnafu' :: Maybe Int -> Maybe (Char, Maybe Int)
toSnafu' Nothing = Nothing
toSnafu' (Just x)
    | q == 0    = Just (toSym x, Nothing)
    | otherwise = Just (toSym r, Just q)
    where
        q = (x + 2) `div` 5
        r = ((x + 2) `mod` 5) - 2

fromSnafu :: String -> Int
fromSnafu = fmap fromSym >>> foldl' (\x n -> x * 5 + n) 0

fromSym :: Char -> Int
fromSym '=' = -2
fromSym '-' = -1
fromSym '0' =  0
fromSym '1' =  1
fromSym '2' =  2

toSym :: Int -> Char
toSym (-2) = '='
toSym (-1) = '-'
toSym   0  = '0'
toSym   1  = '1'
toSym   2  = '2'

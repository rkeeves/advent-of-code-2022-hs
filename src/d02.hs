import           Control.Arrow ((&&&), (>>>))

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> fmap (readPair >>> foldPair lhs rhs >>> (score &&& (snd >>> worth)) >>> sumPair) >>> sum >>> show
    where
        lhs 'A' = R
        lhs 'B' = P
        lhs 'C' = S
        rhs x 'X' = (x, R)
        rhs x 'Y' = (x, P)
        rhs x 'Z' = (x, S)

b :: String -> String
b = lines >>> fmap (readPair >>> foldPair lhs rhs >>> (score &&& (snd >>> worth)) >>> sumPair) >>> sum >>> show
    where
        lhs 'A' = R
        lhs 'B' = P
        lhs 'C' = S
        rhs x 'X' = (x, anti . anti $ x)
        rhs x 'Y' = (x, x)
        rhs x 'Z' = (x, anti x)

data Hand = R | P | S deriving (Eq, Enum, Show)

sumPair :: (Int, Int) -> Int
sumPair = uncurry (+)

foldPair :: (a -> c) -> (c -> b -> d) -> (a, b) -> d
foldPair f g (x, y) = g (f x) y

readPair :: String -> (Char, Char)
readPair [a, ' ', x] = (a, x)

anti :: Hand -> Hand
anti S = R
anti h = succ h

score :: (Hand, Hand) -> Int
score (x, y)
    | x      == anti y = 0
    | anti x == y      = 6
    | otherwise        = 3

worth :: Hand -> Int
worth R = 1
worth P = 2
worth S = 3

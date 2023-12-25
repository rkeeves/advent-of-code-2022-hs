import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.List     (nub, scanl')

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> readHeadSpots >>> followSpots >>> nub >>> length >>> show

b :: String -> String
b = lines >>> readHeadSpots >>> (followSpots `pow` 9) >>> nub >>> length >>> show

type X    = Int
type Y    = Int
type Spot = (X, Y)

pow :: (b -> b) -> Int -> b -> b
pow f n = replicate n f & foldr (.) id

followSpots :: [Spot] -> [Spot]
followSpots = scanl' follow (0, 0)

follow :: Spot -> Spot -> Spot
follow spot goal = let (dx, dy) = goal |-| spot in if abs dx < 2 && abs dy < 2 then spot else spot |+| (signum dx, signum dy)

readHeadSpots :: [String] -> [Spot]
readHeadSpots = concatMap (words >>> readSpots) >>> scanl' (|+|) (0, 0)

readSpots :: [String] -> [Spot]
readSpots ["R", n] = replicate (read n) ( 1, 0)
readSpots ["L", n] = replicate (read n) (-1, 0)
readSpots ["U", n] = replicate (read n) ( 0, 1)
readSpots ["D", n] = replicate (read n) ( 0,-1)

(|+|) :: Spot -> Spot -> Spot
(|+|) = zipSpotWith (+)

(|-|) :: Spot -> Spot -> Spot
(|-|) = zipSpotWith (-)

zipSpotWith :: (Int -> Int -> Int) -> Spot -> Spot ->Spot
zipSpotWith f (x, y) (x', y') = (f x x', f y y')

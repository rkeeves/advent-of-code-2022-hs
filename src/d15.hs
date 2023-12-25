import           Control.Arrow ((&&&), (>>>))
import           Data.Char     (isDigit)
import           Data.Function ((&))
import           Data.List     (foldl', nub, sortOn)
import           Data.Maybe    (mapMaybe)

main :: IO ()
main = interact $ lines >>> fmap readRadarAndBeacon >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [RadarAndBeacon] -> String
a radarAndBeacons = show (pinged - beaconed)
    where y0       = 2_000_000
          beaconed = radarAndBeacons & (fmap beacon >>> filter (y >>> (== y0)) >>> fmap x >>> nub >>> length)
          pinged   = radarAndBeacons & (mapMaybe (diamond >>> cutHorizontalIntervalAt y0) >>> monotonic >>> fmap intervalSize >>> sum)

b :: [RadarAndBeacon] -> String
b radarAndBeacons = unpingedDots & (head >>> (\(Dot x y) -> x * hi + y) >>> show)
    where
        lo             = 0
        hi             = 4_000_000
        diamonds       = diamond <$> radarAndBeacons
        isUnpinged dot = not $ any (dot `isInsideDiamond`) diamonds
        unpingedDots   = diamonds & (fmap (adjustRadius (+ 1)) >>> ascsAndDscBorders >>> uncurry lineIntersections >>> filter (inBoundsOf lo hi) >>> filter isUnpinged)

data Dot               = Dot { x :: !Int, y :: !Int } deriving (Show)
data RadarAndBeacon    = RadarAndBeacon { radar :: !Dot, beacon :: !Dot } deriving (Show)
data Diamond           = Diamond { center :: !Dot, radius :: !Int } deriving (Show)
type Interval          = (Int, Int)
newtype AscendingLine  = AscendingLine Int deriving (Show)
newtype DescendingLine = DescendingLine Int deriving (Show)

adjustRadius :: (Int -> Int) -> Diamond -> Diamond
adjustRadius f (Diamond c r) = Diamond c (f r)

cutHorizontalIntervalAt :: Int -> Diamond -> Maybe Interval
cutHorizontalIntervalAt y0 (Diamond (Dot x y) r)
    | r' >= 0   = Just (x - r',  x + r')
    | otherwise = Nothing
    where r' = r - abs (y0 - y)

monotonic :: [Interval] -> [Interval]
monotonic = foldl' merge [] . sortOn fst
    where merge :: [Interval] -> Interval -> [Interval]
          merge [] next = [next]
          merge all@((lo, hi):rest) next@(lo', hi')
              | hi' <= hi       = all
              | lo' <= (hi + 1) = (lo, hi'):rest
              | otherwise = next:all

intervalSize :: Interval -> Int
intervalSize (xmin, xmax) = xmax - xmin + 1

ascsAndDscBorders :: [Diamond] -> ([AscendingLine], [DescendingLine])
ascsAndDscBorders = concatMap ascBordersOf &&& concatMap dscBordersOf

ascBordersOf :: Diamond -> [AscendingLine]
ascBordersOf diamond = [ascThrough . topDotOf $ diamond, ascThrough . bottomDotOf $ diamond]

dscBordersOf :: Diamond -> [DescendingLine]
dscBordersOf diamond = [dscThrough . topDotOf $ diamond, dscThrough . bottomDotOf $ diamond]

topDotOf :: Diamond -> Dot
topDotOf (Diamond (Dot x y) r) = Dot x (y + r)

bottomDotOf :: Diamond -> Dot
bottomDotOf (Diamond (Dot x y) r) = Dot x (y - r)

ascThrough :: Dot -> AscendingLine
ascThrough (Dot x y) = AscendingLine (y - x)

dscThrough :: Dot -> DescendingLine
dscThrough (Dot x y) = DescendingLine (y + x)

lineIntersections :: [AscendingLine] -> [DescendingLine] -> [Dot]
lineIntersections ascs dscs = concatMap (\asc -> mapMaybe (asc `intersect`) dscs) ascs

intersect :: AscendingLine -> DescendingLine -> Maybe Dot
intersect (AscendingLine a0) (DescendingLine b0)
    | even (b0 - a0) = Just (Dot x y)
    | otherwise      = Nothing
    where x = (b0 - a0) `div` 2
          y = x + a0

inBoundsOf :: Int -> Int -> Dot -> Bool
inBoundsOf lo hi (Dot x y) = lo <= x && x <= hi && lo <= y && y <= hi

isInsideDiamond :: Dot -> Diamond -> Bool
isInsideDiamond dot (Diamond center r) = manhattan dot center <= r

diamond :: RadarAndBeacon -> Diamond
diamond (RadarAndBeacon radar beacon) = Diamond radar (manhattan radar beacon)

manhattan :: Dot -> Dot -> Int
manhattan (Dot x y) (Dot x' y') = abs (x - x') + abs (y - y')

readRadarAndBeacon :: String -> RadarAndBeacon
readRadarAndBeacon = nums >>> (\[x, y, x', y'] -> RadarAndBeacon (Dot x y) (Dot x' y'))
    where
        nums = filter (\c -> isDigit c || c `elem` "-xy") >>> fmap (\ c -> if c `elem` "xy" then ' ' else c) >>> words >>> fmap read

import           Control.Arrow   (first, second, (&&&), (>>>))
import           Data.Char       (ord)
import           Data.Function   ((&))
import           Data.List       (partition, uncons, unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

main :: IO ()
main = interact $ lines >>> readDots >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [Dot] -> String
a = splitStartDotFromRestBy isStart >>> stepsInBsfOrder canReach' >>> dropWhile (dotOfStep >>> not . isGoal) >>> head >>> levelOfStep >>> show
    where
        isStart = chrOfDot >>> (== 'S')
        isGoal  = chrOfDot >>> (== 'E')

b :: [Dot] -> String
b = splitStartDotFromRestBy isStart >>> stepsInBsfOrder (flip canReach') >>> dropWhile (dotOfStep >>> not . isGoal) >>> head >>> levelOfStep >>> show
    where
        isStart = chrOfDot >>> (== 'E')
        isGoal  = chrOfDot >>> (`elem` "aS")

type Pos      = (Int, Int)
type Dot      = (Pos, Char)
type IsGoal   = Dot -> Bool
type CanReach = Dot -> Dot -> Bool
type Queue    = [(Level, Dot)]
type Level    = Int

splitStartDotFromRestBy :: (Dot -> Bool) -> [Dot] -> (Dot, [Dot])
splitStartDotFromRestBy isStart = break isStart >>> ((snd >>> head) &&& (second (drop 1) >>> uncurry (++)))

levelOfStep :: (Level, Dot) -> Level
levelOfStep = fst

dotOfStep :: (Level, Dot) -> Dot
dotOfStep = snd

posOfDot :: Dot -> Pos
posOfDot = fst

chrOfDot :: Dot -> Char
chrOfDot = snd

stepsInBsfOrder ::  CanReach -> (Dot, [Dot]) -> [(Level, Dot)]
stepsInBsfOrder isClose (startDot, dots) = unfoldr (maybeNextBfsStep isClose) (Map.fromList dots, [(0, startDot)])

maybeNextBfsStep :: CanReach -> (Map Pos Char, Queue) -> Maybe ((Level, Dot), (Map Pos Char, Queue))
maybeNextBfsStep canReach (unseen, queue) = case uncons queue of
    Nothing                 -> Nothing
    Just (q@(level, dot), qs) -> dot & (posOfDot >>> neighbors >>> existingAssocsIn unseen >>> filter (canReach dot) >>> (deleteAllFrom unseen &&& enqueueAllInto (level + 1) qs) >>> (q,) >>> Just)

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

existingAssocsIn :: Ord k => Map k v -> [k] -> [(k, v)]
existingAssocsIn unseen = mapMaybe (maybeAssoc unseen)

maybeAssoc :: Ord k => Map k v -> k -> Maybe (k, v)
maybeAssoc unseen pos = (pos,) <$> (unseen Map.!? pos)

deleteAllFrom :: Ord k => Map k v -> [(k, v)] -> Map k v
deleteAllFrom = foldr (fst >>> Map.delete)

enqueueAllInto :: Int -> Queue -> [Dot] -> Queue
enqueueAllInto level queue = fmap (level,) >>> (queue ++)

canReach' :: Dot -> Dot -> Bool
canReach' (_, c) (_, c') = height c' <= (height c + 1)

height :: Char -> Int
height 'E' = height 'z'
height 'S' = height 'a'
height c   = ord c - ord 'a'

readDots :: [String] -> [Dot]
readDots = concatMap (\(y, chs) -> zipWith (\x ch -> ((x, y), ch)) [0 ..] chs) . zip [0 ..]

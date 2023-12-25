import           Control.Arrow (Arrow ((&&&)), (>>>))
import           Data.Function ((&))
import           Data.IntMap   (IntMap)
import qualified Data.IntMap   as IntMap
import           Data.List     (elemIndices, transpose, unfoldr)
import           Data.Maybe    (catMaybes)
import           Data.Sequence (Seq, ViewL (EmptyL, (:<)))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set

main :: IO ()
main = interact $ bothAandB >>> unlines

bothAandB :: String -> [String]
bothAandB txt = let fromAtoB = txt & (readInnerMatrix >>> toValley)
                    fromBtoA = swapStartGoal fromAtoB
                    t0 = 0
                    t1 = timeToGoal t0 fromAtoB
                    t2 = timeToGoal t1 fromBtoA
                    t3 = timeToGoal t2 fromAtoB
               in ["A", show t1, "B", show t3]

type T        = Int
type Matrix a = [[a]]
type X        = Int
type Y        = Int
type Pos      = (X, Y)
type XMax     = Int
type YMax     = Int
type Delta    = Int
type XStorm   = (XMax, Delta, [Int])
type YStorm   = (YMax, Delta, [Int])
data Valley   = Valley { start   :: !Pos
                       , goal    :: !Pos
                       , xmax    :: !XMax
                       , ymax    :: !YMax
                       , xstorms :: !(IntMap (XStorm, XStorm))
                       , ystorms :: !(IntMap (YStorm, YStorm))
                       } deriving (Show)

swapStartGoal :: Valley -> Valley
swapStartGoal (Valley start goal xmax ymax xstorms ystorms) = Valley goal start xmax ymax xstorms ystorms

timeToGoal :: T -> Valley -> T
timeToGoal t v = bfs t v & (last >>> fst)

bfs :: T -> Valley -> [(T, Pos)]
bfs t v = let first = (t, start v) in unfoldr (bfsOnce v) (enqueue [first] Seq.empty, Set.singleton first)

bfsOnce :: Valley -> (Seq (T, Pos), Set (T, Pos)) -> Maybe ((T, Pos), (Seq (T, Pos), Set (T, Pos)))
bfsOnce valley (seq, seen) = case Seq.viewl seq of
    EmptyL  -> Nothing
    x :< xs -> let (seen', xs') = x & (nextsOf >>> filter (\x' -> x' `isValidIn` valley && x' `Set.notMember` seen) >>> (foldr Set.insert seen &&& (`enqueue` xs)))
               in if isGoal valley (snd x) then Just (x, (Seq.empty, seen')) else Just (x, (xs', seen'))

isValidIn :: (T, Pos) -> Valley -> Bool
isValidIn p v = not . isHittingWallOrStormOf v $ p

nextsOf :: (T, Pos) -> [(T, Pos)]
nextsOf (t, pos) =  fmap ((pos |+|) >>> (t + 1,)) [(0, -1), (-1, 0), (0, 1), (1, 0), (0, 0)]

isGoal :: Valley -> Pos -> Bool
isGoal v = (goal v ==)

isStart :: Valley -> Pos -> Bool
isStart v = (start v ==)

isHittingWallOrStormOf :: Valley -> (T, Pos) -> Bool
isHittingWallOrStormOf v p
    | isWithinWall v p = isHittingStorms v p
    | otherwise        = not $ isEdgeCase v p

isWithinWall :: Valley -> (T, Pos) -> Bool
isWithinWall v (_, (x, y)) = 0 <= x && x < xmax v && 0 <= y && y < ymax v

isEdgeCase :: Valley -> (T, Pos) -> Bool
isEdgeCase v (_, p) = isGoal v p || isStart v p

isHittingStorms :: Valley -> (T, Pos) -> Bool
isHittingStorms v (t, (x, y)) = or [ isHittingStorm xs  t x
                                   , isHittingStorm xs' t x
                                   , isHittingStorm ys  t y
                                   , isHittingStorm ys' t y
                                   ]
    where (xs, xs') = v & (xstorms >>> (IntMap.! y))
          (ys, ys') = v & (ystorms >>> (IntMap.! x))

isHittingStorm :: XStorm -> T -> X -> Bool
isHittingStorm (xmax, dx, xs) t x = any ((== x) . abs . (`mod` xmax) . (+ (dx * t))) xs

toValley :: Matrix Char -> Valley
toValley mx = let ymax    = length mx
                  xmax    = length . head $ mx
                  xstorms = xstormsOf xmax mx
                  ystorms = ystormsOf ymax mx
              in Valley (0, -1) (xmax - 1, ymax) xmax ymax xstorms ystorms

xstormsOf :: XMax -> Matrix Char -> IntMap (XStorm, XStorm)
xstormsOf xmax = stormsOf xmax 1 '>' &&& stormsOf xmax (-1) '<' >>> (uncurry zip >>> zip [0..] >>> IntMap.fromList)

ystormsOf :: YMax -> Matrix Char -> IntMap (YStorm, YStorm)
ystormsOf ymax = transpose >>> stormsOf ymax 1 'v' &&& stormsOf ymax (-1) '^' >>> (uncurry zip >>> zip [0..] >>> IntMap.fromList)

stormsOf :: Int -> Delta -> Char -> Matrix Char -> [(Int, Delta, [Int])]
stormsOf themax delta char = fmap (elemIndices char >>> (themax, delta,))

readInnerMatrix :: String -> Matrix Char
readInnerMatrix = lines >>> tail >>> init >>> fmap (tail >>> init)

(|+|) :: Pos -> Pos -> Pos
(|+|) = zipPos (+)

zipPos :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
zipPos f (x, y) (x', y') = (f x x', f y y')

enqueue :: [a] -> Seq a -> Seq a
enqueue xs qs = foldr (flip (Seq.|>)) qs xs

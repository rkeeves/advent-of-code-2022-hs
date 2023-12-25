import           Control.Arrow ((&&&), (***), (>>>))
import           Data.Function ((&))
import           Data.List     (uncons, unfoldr)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set

main :: IO ()
main = interact $ lines >>> concatMap (readTunnelOfDots >>> intersperseMissingDots) >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [Dot] -> String
a dots = searchGraphBy startDot isGoal stackStructure (Set.fromList dots') & (deadEndSteps >>> show)
    where
        startDot           = (500, 0)
        isGoal (_, y)      = y > ymax + 1
        ymax               = maximum $ snd <$> dots'
        dots'              = startDot : dots
        deadEndSteps steps = let successDepth = depth . last $ steps in (length steps - successDepth)

b :: [Dot] -> String
b dots = searchGraphBy startDot isGoal queueStructure (Set.fromList dots') & (nonGoalSteps >>> show)
    where
        startDot      = (500, 0)
        isGoal (_, y) = y > ymax + 1
        ymax          = maximum $ snd <$> dots'
        dots'         = startDot : dots
        nonGoalSteps  = length >>> subtract 1

type Dot               = (Int, Int)
data Step              = Step { depth :: !Int, dot :: !Dot } deriving (Show)
data State b           = State { queue :: !b, seen :: !(Set Dot) } deriving (Show)
data DataStructure a b = DataStructure { zero  :: !b, merge :: !([a] -> b -> b), take :: !(b -> Maybe (a, b)) }

searchGraphBy :: Dot -> (Dot -> Bool) -> DataStructure Step b -> Set Dot -> [Step]
searchGraphBy start isGoal strat@(DataStructure zero merge _) seen = unfoldr (oneStep isGoal strat) startState
  where
    startState = State (merge [Step 1 start] zero) seen

oneStep :: (Dot -> Bool) -> DataStructure Step b -> State b -> Maybe (Step, State b)
oneStep isGoal (DataStructure zero merge take) (State queue seen)
    | Just (step@(Step depth dot), steps) <- take queue = let nexts  = [(0, 1), (-1, 1), (1, 1)] & (fmap (dot |+|) >>> filter (`Set.notMember` seen))
                                                              queue' = nexts & (fmap (Step (depth + 1)) >>> (`merge` steps))
                                                              seen'  = nexts & foldr Set.insert seen
                                                          in Just (step, State (if isGoal dot then zero else queue') seen')
    | otherwise = Nothing

(|+|) :: Dot -> Dot -> Dot
(|+|) (x, y) (x', y') = (x + x', y + y')

intersperseMissingDots :: [Dot] -> [Dot]
intersperseMissingDots xs = concatMap (uncurry section) $ zip xs (tail xs)
    where
        section (x, y) (x', y') = (,) <$> rng x x' <*> rng y y'
        rng a b = [min a b .. max a b]

readTunnelOfDots :: String -> [Dot]
readTunnelOfDots =  words >>> filter (/= "->") >>> fmap (span (',' /=) >>> (read *** (read . tail)))

stackStructure :: DataStructure a [a]
stackStructure = DataStructure [] (++) uncons

queueStructure :: DataStructure a (Seq a)
queueStructure = DataStructure Seq.empty enqueue dequeue

enqueue :: [a] -> Seq a -> Seq a
enqueue xs qs = foldr (flip (Seq.|>)) qs xs

dequeue :: Seq a -> Maybe (a, Seq a)
dequeue qs = case Seq.viewl qs of
    Seq.EmptyL   -> Nothing
    q Seq.:< qs' -> Just (q, qs')

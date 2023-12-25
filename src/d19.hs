import           Control.Arrow (second, (&&&), (>>>))
import           Data.Char     (isDigit)
import           Data.Function ((&))
import           Data.List     (foldl', permutations, transpose)

main :: IO ()
main = interact $ lines >>> fmap readBlueprint >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [BPrint] -> String
a = fmap (second (bestByOrderOfVisit Geo (24, ores, robs) [Ore .. Geo])) >>> fmap (\(id, score) -> id * score) >>> sum >>> show
    where
        ores = fourOf 0
        robs = fourOf 0 |+ Ore

b :: [BPrint] -> String
b = take 3 >>> fmap (snd >>> bestByAllOrdersOfVisit Geo (32, ores, robs) >>> maximum) >>> product >>> show
    where
        ores = fourOf 0
        robs = fourOf 0 |+ Ore

type Four a     = (a, a, a, a)
data Kind       = Ore | Cla | Obs | Geo deriving (Enum, Eq, Show)
type RobCap     = Int
type TimeToLive = Int
type Cost       = Four Int
type Ore        = Int
type Rob        = Int
type Best       = Int
type Id         = Int
type Aim        = Kind
type BPrint     = (Id, (Four RobCap, Four Cost))
data Tree       = Tree { getKind :: Kind, getChildren :: [Tree] }

bestByAllOrdersOfVisit :: Aim -> (TimeToLive, Four Ore, Four Rob) -> (Four RobCap, Four Cost) -> [Best]
bestByAllOrdersOfVisit aim seed env = (\orderOfVisit -> bestByOrderOfVisit aim seed orderOfVisit env) <$> permutations [Ore .. Geo]

bestByOrderOfVisit :: Aim -> (TimeToLive, Four Ore, Four Rob) -> [Kind] -> (Four RobCap, Four Cost) -> Best
bestByOrderOfVisit aim (ttl, ores, robs) orderOfVisit (caps, costs) = orderOfVisit & (unfoldForest >>> bestOfForest (caps, costs, aim) (ttl, ores, robs, 0))

unfoldForest :: [Kind] -> [Tree]
unfoldForest xs = unfoldTree xs <$> xs

unfoldTree :: [Kind] -> Kind -> Tree
unfoldTree xs x = Tree x (unfoldForest xs)

bestOfForest :: (Four RobCap, Four Cost, Aim) -> (TimeToLive, Four Ore, Four Rob, Best) -> [Tree] -> Best
bestOfForest env@(caps, _, aim) (ttl, ores, robs, best)
    | ttl <= 0  = const (ores ? aim)
    | otherwise = filter (getKind >>> \kind -> (kind == aim) || (robs ? kind < caps ? kind)) >>> foldl' (\best' -> bestOfTree env (ttl, ores, robs, best')) best

bestOfTree :: (Four RobCap, Four Cost, Aim) -> (TimeToLive, Four Ore, Four Rob, Best) -> Tree -> Best
bestOfTree env@(caps, costs, aim) (ttl, ores, robs, best) tree@(Tree kind children)
    | ttl <= 0                                      = ores ? aim
    | kind == aim && predictBest'  ttl      <= best = best
    | kind /= aim && predictBest' (ttl - 1) <= best = best
    | cost `isLargerThan` ores                      = bestOfTree   env (ttl - 1, ores |+| robs         , robs        , best) tree
    | otherwise                                     = bestOfForest env (ttl - 1, ores |+| robs |-| cost, robs |+ kind, best) children
    where
        cost         = costs ? kind
        predictBest' = predictBest aim ores robs

predictBest :: Kind -> Four Ore -> Four Rob -> TimeToLive -> Best
predictBest kind ores robs dt = let x0 = ores ? kind
                                    dx = robs ? kind
                                 in x0 + dx * dt + triangular (dt - 1)

triangular :: Int -> Int
triangular n = (n * (n + 1)) `div` 2

readBlueprint :: String -> BPrint
readBlueprint = filter (/= ':') >>> words >>> filter (all isDigit) >>> fmap read >>> parseBlueprint

parseBlueprint :: [Int] -> BPrint
parseBlueprint [id, aa, ba, ca, cb, da, dc] = let ore   = [aa,  0,  0,  0]
                                                  cla   = [ba,  0,  0,  0]
                                                  obs   = [ca, cb,  0,  0]
                                                  geo   = [da,  0, dc,  0]
                                              in (id, capsAndCosts [ore, cla, obs, geo])

capsAndCosts :: [[Int]] -> (Four RobCap, Four Cost)
capsAndCosts = (transpose >>> fmap maximum >>> fourFromList) &&& (fmap fourFromList >>> fourFromList)

fourOf :: a -> Four a
fourOf x = (x, x, x, x)

fourFromList :: [a] -> Four a
fourFromList (a:b:c:d:_) = (a, b, c, d)

(?) :: Four a -> Kind -> a
(?) (x, _, _, _) Ore = x
(?) (_, x, _, _) Cla = x
(?) (_, _, x, _) Obs = x
(?) (_, _, _, x) Geo = x

isLargerThan :: Four Int -> Four Int -> Bool
isLargerThan four = fourZipWith (>) four >>> fourFoldr (||) False

(|+|) :: Four Int -> Four Int -> Four Int
(|+|) = fourZipWith (+)

(|-|) :: Four Int -> Four Int -> Four Int
(|-|) = fourZipWith (-)

(|+) :: Four Int -> Kind -> Four Int
(|+) four k = fourFmapWithKey (\k' -> if k == k' then (+ 1) else id) four

fourZipWith :: (a -> b -> c) -> Four a -> Four b -> Four c
fourZipWith f (a, b, c, d) (a', b', c', d') = (f a a', f b b', f c c', f d d')

fourFmapWithKey :: (Kind -> a -> b) -> Four a -> Four b
fourFmapWithKey f = fourZipWith f (Ore, Cla, Obs, Geo)

fourFoldr :: (a -> b -> b) -> b -> Four a -> b
fourFoldr f x (a, b, c, d) = x & (f d >>> f c >>> f b >>> f a)

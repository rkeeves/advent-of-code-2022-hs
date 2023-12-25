import           Control.Arrow      (first, (&&&), (>>>))
import qualified Data.Bits          as BIT
import           Data.Char          (isAsciiLower)
import           Data.Foldable      (maximumBy)
import           Data.Function      ((&))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IntSet
import           Data.List          (sortBy, unfoldr)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (mapMaybe)
import           Data.Ord           (Down (..), comparing)
import           Data.Set           (Set)
import qualified Data.Set           as Set

main :: IO ()
main = interact $ readValveDatas >>> valvesAndStart "AA" >>> (snd &&& unfoldRateTreeOf) >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: (Valve, Tree Rate) -> String
a = snd >>> scoreTreeAtTime 0 30 >>> subsetScoresOf >>> findBestSubsetAndScore >>> snd >>> show

b :: (Valve, Tree Rate) -> String
b ((startId, _, _), tree) = let subsetsAtT26 = tree & (scoreTreeAtTime 0 26 >>> subsetScores)
                                (bestSubset, scoreT26) = findBestSubsetAndScore subsetsAtT26
                                disjointScoreT26       = maximum $ disjointSubsetScores startId bestSubset subsetsAtT26
                                subsetScores           = foldIntoSubsetScores IntMap.empty
                            in show (scoreT26 + disjointScoreT26)

type Name              = String
type IdBitmask         = Int
type T                 = Int
type Rate              = Int
type Score             = Int
type Distance          = Int
type RawValve          = (Name, Rate, [Name])
type DistancedValve    = (Name, Rate, [(Name, Distance)])
type Valve             = (IdBitmask, Rate, [(IdBitmask, Distance)])
data Tree a            = Tree { val :: a, time :: T, path :: [IdBitmask], children :: [Tree a] }
type SubsetBits        = Int
type ValveByIdBitmask  = IntMap Valve
type ScoreBySubsetBits = IntMap Score

findBestSubsetAndScore :: ScoreBySubsetBits -> (SubsetBits, Score)
findBestSubsetAndScore = IntMap.assocs >>> sortBy (comparing (snd >>> Down)) >>> head

disjointSubsetScores :: IdBitmask -> SubsetBits -> IntMap Score -> [Score]
disjointSubsetScores startNodeBit subset = IntMap.assocs >>> filter (fst >>> (subset BIT..&.) >>> (`BIT.clearBit` startNodeBit) >>> (== 0)) >>> fmap snd

subsetScoresOf :: Tree Score -> ScoreBySubsetBits
subsetScoresOf = foldIntoSubsetScores IntMap.empty

foldIntoSubsetScores :: ScoreBySubsetBits -> Tree Score -> ScoreBySubsetBits
foldIntoSubsetScores accu (Tree score time path children) = let accu' = IntMap.insertWith max (subsetBitsOf path) score accu
                                                            in foldr (flip foldIntoSubsetScores) accu' children

subsetBitsOf :: [IdBitmask] -> SubsetBits
subsetBitsOf = foldr (flip BIT.setBit) 0

scoreTreeAtTime :: Int -> T -> Tree Rate -> Tree Score
scoreTreeAtTime score endtime (Tree rate t path children) = let score'    = score + abs ((endtime - t) * rate)
                                                                children' = children & (filter (time >>> (< endtime)) >>> fmap (scoreTreeAtTime score' endtime))
                                                            in Tree score' t path children'

unfoldRateTreeOf :: (ValveByIdBitmask, Valve) -> Tree Rate
unfoldRateTreeOf (valves, startValve) = unfoldRateTree valves 0 [] startValve

unfoldRateTree :: ValveByIdBitmask -> T -> [IdBitmask] -> Valve -> Tree Rate
unfoldRateTree vs t path (id, rate, ids) = let path'    = id : path
                                               nexts    = mapMaybe (\(id', d) -> fmap (id', d,) $ vs IntMap.!? id') ids
                                               children = (\(id', dt, v) -> unfoldRateTree (IntMap.delete id' vs) (t + dt + 1) path' v) <$> nexts
                                           in Tree rate t path' children

valvesAndStart :: Name -> Map Name (Name, Rate, [Name]) -> (ValveByIdBitmask, Valve)
valvesAndStart startName m = let usedNames = m & (Map.elems >>> filter (\(name, rate, _) -> rate > 0 || name == startName) >>> fmap (\(name, _, _) -> name))
                                 ids       = Map.fromList $ zip usedNames [0..]
                                 distances = m & (toShortestDistanceMap >>> filterDistanceMap (`elem` usedNames) >>> relabelNamesToIds ids)
                                 startId   = ids Map.! startName
                             in (IntMap.delete startId distances, distances IntMap.! startId)

relabelNamesToIds :: Map Name IdBitmask -> Map Name DistancedValve -> ValveByIdBitmask
relabelNamesToIds ids = Map.foldr (relabelOne ids) IntMap.empty

relabelOne :: Map Name IdBitmask -> DistancedValve -> ValveByIdBitmask -> ValveByIdBitmask
relabelOne ids (name, rate, edges) = let id     = ids Map.! name
                                         edges' = first (ids Map.!) <$> edges
                                     in IntMap.insert id (id, rate, edges')

filterDistanceMap :: (Name -> Bool) -> Map Name DistancedValve -> Map Name DistancedValve
filterDistanceMap test = Map.filterWithKey (\k _ -> test k) >>> Map.map (filterEdge test)

filterEdge :: (Name -> Bool) -> DistancedValve -> DistancedValve
filterEdge f (name, rate, edges) = (name, rate, filter (\(name, _) -> f name) edges)

toShortestDistanceMap :: Map Name RawValve -> Map Name DistancedValve
toShortestDistanceMap m = Map.map (bfsDistances m) m

bfsDistances :: Map Name RawValve -> (Name, Rate, [Name]) -> DistancedValve
bfsDistances m (name, rate, names) = let edges' = unfoldr (bfsDistance m) ([(name, 0)], Set.singleton name) in (name, rate, edges')

bfsDistance :: Map Name RawValve -> ([(Name, Distance)], Set Name) -> Maybe ((Name, Distance), ([(Name, Distance)], Set Name))
bfsDistance m ([], _) = Nothing
bfsDistance m (cur@(name, d):queue, seen) = m & ((Map.! name) >>> edges >>> filterUnseen seen >>> (enqueue (d + 1) queue &&& addToSeen seen) >>> (cur,) >>> Just)

edges :: RawValve -> [Name]
edges (_, _, x) = x

filterUnseen ::  Set Name -> [Name] -> [Name]
filterUnseen seen = filter (`Set.notMember` seen)

addToSeen ::  Set Name -> [Name] -> Set Name
addToSeen = foldr Set.insert

enqueue :: Int -> [(Name, Distance)] -> [Name] -> [(Name, Distance)]
enqueue dist queue = fmap (, dist) >>> (queue ++)

readValveDatas :: String -> Map Name (Name, Rate, [Name])
readValveDatas = lines >>> fmap (readValveData >>> (\all@(name, _, _) -> (name, all))) >>> Map.fromList

readValveData :: String -> (Name, Rate, [Name])
readValveData s = let (valve:id:has:flow:rate:x:tunnels:lead:to:valves:ids) = tokens s in (id, read x, ids)

tokens :: String -> [String]
tokens = fmap (\c -> if c == '=' then ' ' else c) >>> filter (`notElem` ";,") >>> words

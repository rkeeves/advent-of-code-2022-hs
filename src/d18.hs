import           Control.Arrow ((>>>))
import           Data.List     (unfoldr)
import           Data.Set      (Set)
import qualified Data.Set      as Set

main :: IO ()
main = interact $ lines >>> fmap readVec >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [Vec] -> String
a = edges >>> unidirectionals >>> length >>> show

b :: [Vec] -> String
b = dfsHitSurfaces >>> sum >>> show

type X         = Int
type Y         = Int
type Z         = Int
type Vec       = (X, Y, Z)
type Cube      = (Vec, Vec)
type Edge      = (Vec, Vec)
type State     = ([Vec], Set Vec)
type IsOutside = (Vec -> Bool)
type IsLava    = (Vec -> Bool)

dfsHitSurfaces :: [Vec] -> [Int]
dfsHitSurfaces vecs = unfoldr (dfsOnce isOutside isLava) ([start], Set.singleton start)
    where
        cube@(start, _) = stretch 1 . cubeOf $ vecs
        isOutside vec   = vec `outside` cube
        isLava vec      = vec `Set.member` rocks
        rocks           = Set.fromList vecs

dfsOnce :: IsOutside -> IsLava -> State -> Maybe (Int, State)
dfsOnce isOutside isLava ([], _)      = Nothing
dfsOnce isOutside isLava (vec:vecs, seen) = Just (rocks, (vecs', seen'))
    where
        adjs    = (|+| vec) <$> directions
        isBad x = isOutside x || isLava x || x `Set.member` seen
        nexts   = filter (not . isBad) adjs
        rocks   = length . filter isLava $ adjs
        vecs'   = nexts ++ vecs
        seen'   = foldr Set.insert seen nexts

cubeOf :: [Vec] -> Cube
cubeOf (v:vs) = (foldr (zipvec min) v vs, foldr (zipvec max) v vs)

stretch :: Int -> Cube -> Cube
stretch n (lo, hi) = (lo |+| (-n, -n, -n), hi |+| (n, n, n))

outside :: Vec -> Cube -> Bool
outside (x, y, z) ((xmin, ymin, zmin), (xmax, ymax, zmax)) = x < xmin || xmax < x || y < ymin || ymax < y || z < zmin || zmax < z

unidirectionals :: [Edge] -> Set Edge
unidirectionals = foldr miss Set.empty
    where miss (a, b) set = if (b, a) `Set.member` set then Set.delete (b, a) set else Set.insert (a, b) set

edges :: [Vec] -> [Edge]
edges = concatMap neighbors

neighbors :: Vec -> [Edge]
neighbors v = fmap (\dir -> (v, v |+| dir)) directions

directions :: [Vec]
directions = [ (  1,  0,  0)
             , ( -1,  0,  0)
             , (  0,  1,  0)
             , (  0, -1,  0)
             , (  0,  0,  1)
             , (  0,  0, -1)
             ]

(|+|) :: Vec -> Vec -> Vec
(|+|) = zipvec (+)

zipvec :: (Int -> Int -> Int) -> Vec -> Vec -> Vec
zipvec f (x, y, z) (x', y', z') = (f x x', f y y', f z z')

readVec :: String -> Vec
readVec = (\[x, y, z] -> (x, y, z)) . fmap read . words . fmap (\c -> if c == ',' then ' ' else c)

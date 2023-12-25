import           Control.Arrow (first, second, (&&&), (***), (>>>))
import           Data.Char     (isDigit)
import           Data.Function (on, (&))
import           Data.List     (break, foldl', groupBy, mapAccumL, minimumBy,
                                nub, sortBy, unfoldr)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (catMaybes, fromJust)
import           Data.Set      (Set)
import qualified Data.Set      as Set

data Cmd     = L | R | M deriving (Show)
data Cell    = Free | Wall deriving (Eq, Show)
type V2      = (Int, Int)
type V3      = (Int, Int, Int)
type Size    = Int
type C       = Int
type R       = Int
type CR      = (C, R)
type Dir     = V2
type D2      = (CR, Dir)
type XYZ     = V3
-- | U V N = Main Tangential Normal
type U       = V3
type V       = V3
type N       = V3
type D3      = (XYZ, (U, V))

main :: IO ()
main = interact $ readGridAndCmds >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: (Map CR Cell, [Cmd]) -> String
a (cells, cmds) = show (1000 * (row + 1) + 4 * (col + 1) + dirScore dir)
    where
        start2d         = (findStart cells, (1, 0))
        ((col, row), dir) = mazePlaneMoves (columnMinMaxs cells, rowMinMaxs cells, cells) start2d cmds

b :: (Map CR Cell, [Cmd]) -> String
b (cells, cmds) = show (1000 * (row + 1) + 4 * (col + 1) + dirScore dir)
    where
        size          = findSideLength cells
        start2d       = (findStart cells, (1, 0))
        start3d       = ((0, 0, 0), ((1, 0, 0), (0, 1, 0)))
        bijection2d3d = colAndRowToXYZAndFaceNormal <$> dfsInLockstepOnPlaneAndCube size cells start2d start3d
        map3dToCells  = toMap3dToCells cells bijection2d3d
        (xyz, (u, v)) = mazeCubeMoves size map3dToCells start3d cmds
        map3dTo2d     = toMap3dTo2d bijection2d3d
        (col, row)    = map3dTo2d Map.! (xyz, u × v)
        dir           = findDirOfFinal map3dTo2d (xyz, (u, v)) (col, row)

dirScore :: (Int, Int) -> Int
dirScore ( 1,  0) = 0
dirScore ( 0,  1) = 1
dirScore (-1,  0) = 2
dirScore ( 0, -1) = 3

findDirOfFinal :: Map (XYZ, N) CR -> D3 -> CR -> Dir
findDirOfFinal map3dTo2d (xyz, (u, v)) cr  = head . catMaybes $ [maybeDirFromFront, maybeDirFromBack]
    where
        -- | Assumes that there's a point - on your current face - in front of you or behind you, e.g. breaks on an 1 x 1 x 1
        maybeDirFromFront = fmap (vmap signum . (|-| cr)) $ map3dTo2d Map.!? (xyz |+| u, u × v)
        maybeDirFromBack  = fmap (vmap signum . (cr |-|)) $ map3dTo2d Map.!? (xyz |-| u, u × v)

columnMinMaxs :: Map CR Cell -> Map C (Int, Int)
columnMinMaxs m = Map.fromList . fmap (\all@((c, _):_) -> (c, minmax (snd <$> all))) . groupBy (on (==) fst) . sortBy (on compare fst) $ Map.keys m

rowMinMaxs :: Map CR Cell -> Map R (Int, Int)
rowMinMaxs m = Map.fromList . fmap (\all@((_, r):_) -> (r, minmax (fst <$> all))) . groupBy (on (==) snd) . sortBy (on compare snd) $ Map.keys m

minmax :: [Int] -> (Int, Int)
minmax = minimum &&& maximum

mazePlaneMoves :: (Map C (Int, Int), Map R (Int, Int), Map CR Cell) -> D2 -> [Cmd] -> D2
mazePlaneMoves env = foldl' (mazePlaneMove env)

mazePlaneMove :: (Map C (Int, Int), Map R (Int, Int), Map CR Cell) -> D2 -> Cmd -> D2
mazePlaneMove _            d2 L = planeMove d2 L
mazePlaneMove _            d2 R = planeMove d2 R
mazePlaneMove (mc, mr, cs) d2 M = case cs Map.!? cr' of
    Just Free -> d2'
    Just Wall -> d2
    Nothing   -> case dir of
        ( 1,  0) -> let cr'' = (cmin, r) in if Just Free == (cs Map.!? cr'') then (cr'', dir) else d2
        (-1,  0) -> let cr'' = (cmax, r) in if Just Free == (cs Map.!? cr'') then (cr'', dir) else d2
        ( 0,  1) -> let cr'' = (c, rmin) in if Just Free == (cs Map.!? cr'') then (cr'', dir) else d2
        ( 0, -1) -> let cr'' = (c, rmax) in if Just Free == (cs Map.!? cr'') then (cr'', dir) else d2
    where
        d2'@(cr', dir')  = planeMove d2 M
        ((c, r), dir)    = d2
        (rmin, rmax)     = mc Map.! c
        (cmin, cmax)     = mr Map.! r

mazeCubeMoves :: Size -> Map (XYZ, N) Cell -> D3 -> [Cmd] -> D3
mazeCubeMoves s m = foldl' (mazeCubeMove s m)

mazeCubeMove :: Size -> Map (XYZ, N) Cell -> D3 -> Cmd -> D3
mazeCubeMove s m d3 cmd = let d3'@(xyz, (u, v)) = cubeMove s d3 cmd in if m Map.! (xyz, u × v) == Free then d3' else d3

toMap3dToCells :: Map CR Cell ->  [(CR, (XYZ, N))] -> Map (XYZ, N) Cell
toMap3dToCells m xs = Map.fromList [ (key, m Map.! cr) | (cr, key) <- xs ]

toMap3dTo2d :: [(CR, (XYZ, N))] -> Map (XYZ, N) CR
toMap3dTo2d xs = xs & (fmap (\(a, b) -> (b, a)) >>> Map.fromList)

colAndRowToXYZAndFaceNormal :: (D2, D3) -> (CR, (XYZ, N))
colAndRowToXYZAndFaceNormal ((cr, _), (xyz, (u, v))) = (cr, (xyz, u × v))

dfsInLockstepOnPlaneAndCube :: Size -> Map CR Cell -> D2 -> D3 -> [(D2, D3)]
dfsInLockstepOnPlaneAndCube s m d2@(cr, _) d3 = unfoldr (dfsLockstepOnce s) ([(d2, d3)], Map.delete cr m)

dfsLockstepOnce :: Size -> ([(D2, D3)], Map CR Cell) -> Maybe ((D2, D3), ([(D2, D3)], Map CR Cell))
dfsLockstepOnce _ ([],   _  ) = Nothing
dfsLockstepOnce s (q:qs, m)   = let fourHeadings = [[M], [R, M], [L, M], [L, L, M]]
                                in fourHeadings & (fmap (planeCubeMoves s q) >>> filter (`isPresentIn` m) >>> ((++ qs) &&& (`remFrom` m)) >>> (q,) >>> Just)

isPresentIn :: (D2, D3) -> Map CR Cell -> Bool
isPresentIn ((cr, _), _) m = cr `Map.member` m

remFrom :: [(D2, D3)] -> Map CR Cell -> Map CR Cell
remFrom xs m = xs & (fmap (fst >>> fst) >>> foldr Map.delete m)

planeCubeMoves :: Size -> (D2, D3) -> [Cmd] -> (D2, D3)
planeCubeMoves s (d2, d3) cmds = (planeMoves d2 cmds, cubeMoves s d3 cmds)

planeMoves :: D2 -> [Cmd] -> D2
planeMoves = foldl' planeMove

planeMove :: D2 -> Cmd -> D2
planeMove (x, (i, j)) L = (x       , ( j, -i))
planeMove (x, (i, j)) R = (x       , (-j,  i))
planeMove (x, dx)     M = (x |+| dx, dx      )

cubeMoves :: Size ->  D3 -> [Cmd] -> D3
cubeMoves s = foldl' (cubeMove s)

cubeMove :: Size -> D3 -> Cmd -> D3
cubeMove s (p, (u, v)) L = (p, (v × u × u, u            ))
cubeMove s (p, (u, v)) R = (p, (v            , u × v × v))
cubeMove s (p, (u, v)) M = let p' = p |+| u in if isAll (\x -> 0 <= x && x < s) p' then (p', (u, v)) else (p, (u × v, v))

findSideLength :: Map CR Cell -> Size
findSideLength m = m & (Map.size >>> (`div` 6) >>> fromIntegral >>> sqrt >>> floor)

findStart :: Map CR Cell -> CR
findStart m = minimumBy (\(r, c) (r', c') -> c `compare` c' <> r `compare` r' ) (Map.keys m)

readGridAndCmds :: String -> (Map CR Cell, [Cmd])
readGridAndCmds = lines >>> break (== "") >>> (readGrid *** ((!! 1) >>> readCmds))

readGrid :: [[Char]] -> Map CR Cell
readGrid rs = Map.fromList $ concat [ catMaybes [ maybeCell (c, r) ch | (c, ch) <- zip [0..] cs ] | (r, cs) <- zip [0..] rs ]

maybeCell :: CR -> Char -> Maybe (CR, Cell)
maybeCell cr '.' = Just (cr, Free)
maybeCell cr '#' = Just (cr, Wall)
maybeCell cr _   = Nothing

readCmds :: String -> [Cmd]
readCmds = unfoldr readCmd >>> concat

readCmd :: String -> Maybe ([Cmd], String)
readCmd []      = Nothing
readCmd ('L':s) = Just ([L], s)
readCmd ('R':s) = Just ([R], s)
readCmd s       = s & (span isDigit >>> first (read >>> flip replicate M) >>> Just)

instance Vector V2 where
  vmap :: (Int -> Int) -> V2 -> V2
  vmap f (x, y) = (f x, f y)
  vzip :: (Int -> Int -> Int) -> V2 -> V2 -> V2
  vzip f (x, y)  (x', y') = (f x x', f y y')
  isAll :: (Int -> Bool) -> V2 -> Bool
  isAll f (x, y) = f x && f y

instance Vector V3 where
  vmap :: (Int -> Int) -> V3 -> V3
  vmap f (x, y, z) = (f x, f y, f z)
  vzip :: (Int -> Int -> Int) -> V3 -> V3 -> V3
  vzip f (x, y, z)  (x', y', z') = (f x x', f y y', f z z')
  isAll :: (Int -> Bool) -> V3 -> Bool
  isAll f (x, y, z) = f x && f y && f z

(×) :: V3 -> V3 -> V3
(×) (x, y, z) (x', y', z') = ( y * z' - z * y'
                               , z * x' - x * z'
                               , x * y' - y * x')

class Vector v where
    vmap :: (Int -> Int) -> v -> v
    vzip :: (Int -> Int -> Int) -> v -> v -> v
    (|+) :: v -> Int -> v
    (|+) v k = vmap (+ k) v
    (|-) :: v -> Int -> v
    (|-) v k = vmap (subtract k) v
    (|*) :: v -> Int -> v
    (|*) v k = vmap (* k) v
    (|+|) :: v -> v -> v
    (|+|) = vzip (+)
    (|-|) :: v -> v -> v
    (|-|) = vzip (-)
    (|*|) :: v -> v -> v
    (|*|) = vzip (*)
    isAll :: (Int -> Bool) -> v -> Bool

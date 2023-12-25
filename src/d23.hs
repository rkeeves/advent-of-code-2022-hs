import           Control.Arrow ((>>>))
import           Data.Foldable (asum)
import           Data.Function ((&))
import           Data.List     (unfoldr)
import           Data.Maybe    (catMaybes, isNothing)
import           Data.Set      (Set)
import qualified Data.Set      as Set

main :: IO ()
main = interact $ readElves >>> Set.fromList >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: Set Pos -> String
a = ([N, S, W, E],) >>> diffuseTillStable >>> (!! 10) >>> countFreeSpots >>> show

b :: Set Pos -> String
b = ([N, S, W, E],) >>> diffuseTillStable >>> length >>> show

type X   = Int
type Y   = Int
type Pos = (X, Y)
type Dst = Pos
type Src = Pos
data Dir = N | NE | E | SE | S | SW | W | NW deriving (Enum, Show)

diffuseTillStable :: ([Dir], Set Pos) -> [Set Pos]
diffuseTillStable state@(dirs, start) = start : unfoldr maybeDiffuseOnce state

maybeDiffuseOnce :: ([Dir], Set Pos) -> Maybe (Set Pos, ([Dir], Set Pos))
maybeDiffuseOnce (dirs@(d:ds), srcs) = (\x -> (x, (ds ++ [d], x))) <$> maybeNexts
    where
        maybeNexts = dirs & (proposeds srcs >>> uncurry resolve)

proposeds :: Set Src -> [Dir] -> ([Src], [(Dst, Src)])
proposeds srcs dirs = srcs & (Set.toList >>> foldr f ([], []))
    where
        f src (frozens, edges) = case maybePropose srcs dirs src of
            Just dst -> (      frozens, (src, dst) : edges)
            Nothing  -> (src : frozens,              edges)

maybePropose :: Set Src -> [Dir] -> Src -> Maybe Dst
maybePropose srcs dirs src = let maybeProposals = maybeProposeDir srcs src <$> dirs
                             in if any isNothing maybeProposals then asum maybeProposals else Nothing

maybeProposeDir :: Set Src -> Src -> Dir -> Maybe Dst
maybeProposeDir srcs src dir = if isUnproposable srcs src dir then Nothing else Just (src |+| posOf dir)

isUnproposable :: Set Src -> Src -> Dir -> Bool
isUnproposable srcs src = threeNeighbors >>> any ((src |+|) >>> (`Set.member` srcs))

resolve :: [Src] -> [(Dst, Src)] -> Maybe (Set Pos)
resolve frozens []    = Nothing
resolve frozens edges = Just (foldr collideWithOpposingSideOrWin (Set.fromList frozens) edges)
    where
        collideWithOpposingSideOrWin (src, dst) set
            | dst `Set.member` set = set & (Set.delete dst >>> Set.insert (dst |+| (dst |-| src)) >>> Set.insert src)
            | otherwise            = Set.insert dst set

posOf :: Dir -> Pos
posOf N  = ( 0, -1)
posOf NE = ( 1, -1)
posOf E  = ( 1,  0)
posOf SE = ( 1,  1)
posOf S  = ( 0,  1)
posOf SW = (-1,  1)
posOf W  = (-1,  0)
posOf NW = (-1, -1)

threeNeighbors :: Dir -> [Pos]
threeNeighbors dir =  posOf <$> [clock dir, dir, counterClock dir]

clock :: Dir -> Dir
clock NW = N
clock x  = succ x

counterClock :: Dir -> Dir
counterClock N = NW
counterClock x = pred x

countFreeSpots :: Set Pos -> Int
countFreeSpots ps = ps & (Set.toList >>> boundingRect >>> area >>> subtract (length ps))

area :: (Pos, Pos) -> Int
area ((x0, y0), (x1, y1)) = (abs (x1 - x0) + 1) * (abs (y1 - y0) + 1)

boundingRect :: [Pos] -> (Pos, Pos)
boundingRect (p:ps) = (foldr (zipPosWith min) p ps, foldr (zipPosWith max) p ps)

(|+|) :: Pos -> Pos -> Pos
(|+|) = zipPosWith (+)

(|-|) :: Pos -> Pos -> Pos
(|-|) = zipPosWith (-)

zipPosWith :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
zipPosWith f (x, y) (x', y') = (f x x', f y y')

readElves :: String -> [Pos]
readElves = lines >>> readMatrix (\x y c -> if c == '#' then Just (x, y) else Nothing)

readMatrix :: (X -> Y -> Char -> Maybe a) -> [String] -> [a]
readMatrix f css = [ [ f x y c | (x, c) <- zip [0..] cs ] | (y, cs) <- zip [0..] css] & (concat >>> catMaybes)

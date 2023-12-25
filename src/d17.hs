import           Control.Arrow   ((>>>))
import           Data.List       (unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)
import           Data.Set        (Set)
import qualified Data.Set        as Set

main :: IO ()
main = interact $ lines >>> head >>> fmap readJet >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [Jet] -> String
a = infiniteSteps basicRocks >>> zip [1..] >>> predictHeightAtN 2022 Map.empty >>> show

b :: [Jet] -> String
b = infiniteSteps basicRocks >>> zip [1..] >>> predictHeightAtN 1_000_000_000_000 Map.empty >>> show

type X         = Int
type Y         = Int
type N         = Int
type Gif       = [[Char]]
type Pos       = (X, Y)
type JetId     = Int
type Jet       = Pos
type RockId    = Int
type Rock      = [Pos]
type Tower     = Set Pos
data Step      = Step { ymax   :: Y, rockId :: RockId , jetId  :: JetId , tower  :: Tower }
type State     = (Y, Tower, [(RockId, Rock)], [(JetId, Jet)])

predictHeightAtN :: N -> Map (RockId, JetId) (N, Y) -> [(N, Step)] -> Y
predictHeightAtN nGoal cache ((n, Step y ri ji tower):steps)
    | nGoal == n                                = y
    | Just guess <- predict (cache Map.!? key)  = guess
    | otherwise                                 = predictHeightAtN nGoal cache' steps
    where
       predict Nothing         = Nothing
       predict (Just (n', y')) = let (d, m) = (nGoal - n) `divMod` (n - n')
                                 in if m == 0 then Just (y + d * (y - y')) else Nothing
       cache' = Map.insert key (n, y) cache
       key    = (ri, ji)

infiniteSteps :: [Rock] -> [Jet] -> [Step]
infiniteSteps rocks jets = let y0     = 0
                               tower  = Set.empty
                               rocks' = cycle . zip [0..] $ rocks
                               jets'  = cycle . zip [0..] $ jets
                           in unfoldr placeOneMoreRock (y0, tower, rocks', jets')

placeOneMoreRock :: State -> Maybe (Step, State)
placeOneMoreRock (y, tower, (ri, rock):rs, jets) = Just (step, state)
    where
       rock'                = fmap (|+| (2, 4 + y)) rock
       (ji , jets', rock'') = settleRock tower rock' jets
       tower'               = foldr Set.insert tower rock''
       ys                   = snd <$> rock''
       y'                   = maximum (y:ys)
       step                 = Step y' ri ji tower'
       state                = (y', tower', rs, jets')

settleRock :: Tower -> Rock -> [(JetId, Jet)] -> (JetId, [(JetId, Jet)], Rock)
settleRock tower rock ((ji, jet):js)
    | tower `overlaps` rock'' = (ji, js, rock')
    | otherwise               = settleRock tower rock'' js
    where
       rock'  = let newRock = fmap (|+| jet) rock in if tower `overlaps` newRock then rock else newRock
       rock'' = fmap (|+| (0, -1)) rock'
       overlaps  tower            = any (overlaps' tower)
       overlaps' tower pos@(x, y) = x < 0 || 6 < x || y <= 0 || pos `Set.member` tower

(|+|) :: Pos -> Pos -> Pos
(|+|) (x, y) (x', y') = (x + x', y + y')

readJet :: Char -> Jet
readJet '<' = (-1, 0)
readJet '>' = ( 1, 0)

basicRocks :: [Rock]
basicRocks = fromGif (,) <$> gifs

fromGif :: (X -> Y -> a) -> Gif -> [a]
fromGif f = reverse >>> zipWith (\y ->  zipWith (\x c -> if c == '#' then Just (f x y) else Nothing) [0..] >>> catMaybes) [0..] >>> concat

gifs :: [Gif]
gifs = [ gif0, gif1, gif2, gif3, gif4 ]

gif0 :: Gif
gif0 = ["####"]

gif1 :: Gif
gif1 = [" # "
       ,"###"
       ," # "]

gif2 :: Gif
gif2 = ["  #"
       ,"  #"
       ,"###"]

gif3 :: Gif
gif3 = ["#"
       ,"#"
       ,"#"
       ,"#"]

gif4 :: Gif
gif4 = ["##"
       ,"##"]

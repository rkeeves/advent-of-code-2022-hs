import           Control.Arrow   ((>>>))
import           Data.Foldable   (toList)
import           Data.Function   ((&))
import           Data.List       (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq

main :: IO ()
main = interact $ lines >>> fmap read >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [Int] -> String
a xs = ids & (Seq.fromList >>> shiftAll ixs >>> toList >>> fmap (originals Map.!) >>> numsAt [1000, 2000, 3000] >>> sum >>> show)
    where
        ixs       = zip [0..] xs
        originals = Map.fromList ixs
        ids       = Map.keys originals

b :: [Int] -> String
b xs = ids & (Seq.fromList >>> iterate (shiftAll ixs) >>> (!! 10) >>> toList >>> fmap (originals Map.!) >>> numsAt [1000, 2000, 3000] >>> sum >>> show)
    where
        ixs       = xs & (fmap (* 811_589_153) >>> zip [0..])
        originals = Map.fromList ixs
        ids       = Map.keys originals

numsAt :: [Int] -> [Int] -> [Int]
numsAt indexes xs = let n    = length xs
                        xs'  = dropWhile (/= 0) . cycle $ xs
                    in (xs' !!) . (`mod` n) <$> indexes

type Id = Int
type MoveBy = (Int, Int)

shiftAll :: [MoveBy] -> Seq Id -> Seq Id
shiftAll moves qs = foldl' shift qs moves

shift :: Seq Id -> MoveBy -> Seq Id
shift qs (id, k) = let src = fromJust $ Seq.findIndexL (== id) qs
                       n   = Seq.length qs
                       dst = (src + k) `mod` (n - 1)
                   in Seq.insertAt dst id . Seq.deleteAt src $ qs

import           Control.Arrow      ((&&&), (>>>))
import           Data.Char          (isDigit)
import           Data.Function      ((&))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (foldl', sortBy, stripPrefix, unfoldr)
import           Data.Maybe         (fromJust, fromMaybe)

main :: IO ()
main = interact $ lines >>> readMonkeysAndItems >>> (\x -> ["A", uncurry a x, "B", uncurry b x]) >>> unlines

a :: [Monkey] -> [Item] -> String
a monkeys = concatMap (rounds 20 monkeys' >>> inspections) >>> sumByMonkeys >>>  IntMap.elems >>> descending >>> take 2 >>> product >>> show
    where
        monkeys' = fmap (\monkey -> monkey { adjust = (`div` 3) }) monkeys

b :: [Monkey] -> [Item] -> String
b monkeys = concatMap (rounds 10_000 monkeys' >>> inspections) >>> sumByMonkeys >>> IntMap.elems >>> descending >>> take 2 >>> product >>> show
    where
        lcmd     = lcmD monkeys
        monkeys' = fmap (\monkey -> monkey { adjust = (`mod` lcmd) }) monkeys

type X      = Int
type Id     = Int
type Div    = Int
type Item   = (Id, X)
type Op     = (X -> X -> X)
data Monkey = Monkey { id :: !Id, op :: !(Op, Maybe Int), adjust :: !(X -> X), d :: !Div , t :: !Id, f :: !Id }

descending :: [Int] -> [Int]
descending = sortBy (flip compare)

sumByMonkeys :: [Id] -> IntMap Int
sumByMonkeys = fmap (, 1) >>> IntMap.fromListWith (+)

inspections :: (Item, [Id]) -> [Id]
inspections = snd

rounds :: Int -> [Monkey] -> Item -> (Item, [Id])
rounds n ms item = foldl' inspect (item, []) (ms & (replicate n >>> concat))

inspect :: (Item, [Id]) -> Monkey -> (Item, [Id])
inspect (item@(id, x), ids) (Monkey m (op, maybeY) adjust d t f)
    | id /= m   = (item, ids)
    | otherwise = let x'  = maybeY & (fromMaybe x >>> (x `op`) >>> adjust)
                      id' = if x' `mod` d == 0 then t else f
                  in ((id', x'), id : ids)

lcmD :: [Monkey] -> Div
lcmD (m:ms) = foldr (lcm . d) (d m) ms

readMonkeysAndItems :: [String] -> ([Monkey], [Item])
readMonkeysAndItems = chunksOf 7 >>> fmap readMonkeyItem >>> fmap snd &&& concatMap fst

readMonkeyItem :: [String] -> ([Item], Monkey)
readMonkeyItem (id : items : op : div : true : false : _) = let id'    = id & (stripPrefix "Monkey " >>> fromJust >>> parseId)
                                                                items' = items & (stripPrefix "  Starting items: " >>> fromJust >>> parseItems id')
                                                                op'    = op & (stripPrefix "  Operation: new = old " >>> fromJust >>> parseOp)
                                                                div'   = div & (stripPrefix "  Test: divisible by " >>> fromJust >>> read)
                                                                true'  = true & (stripPrefix "    If true: throw to monkey " >>> fromJust >>> read)
                                                                false' = false & (stripPrefix "    If false: throw to monkey " >>> fromJust >>> read)
                                                            in (items', Monkey id' op' Prelude.id div' true' false')

parseId :: String -> Int
parseId = filter isDigit >>> read

parseItems :: Int -> String -> [Item]
parseItems id = filter (/= ',') >>> words >>> fmap (read >>> (id,))

parseOp :: String -> (Op, Maybe Int)
parseOp = words >>> readOp

readOp :: [String] -> (Op, Maybe Int)
readOp ["+", "old"] = ((+), Nothing)
readOp ["+", s]     = ((+), Just (read s))
readOp ["*", "old"] = ((*), Nothing)
readOp ["*", s]     = ((*), Just (read s))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if n < 0 || null xs then Nothing else Just (splitAt n xs))

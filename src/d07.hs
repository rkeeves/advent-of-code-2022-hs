import           Control.Arrow   ((>>>))
import           Data.Char       (isDigit)
import           Data.Function   ((&))
import           Data.List       (stripPrefix, tails)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> fmap readCmd >>> zipReversePaths >>> toFileMap >>> toDirSizes >>> filter (<= 100_000) >>> sum >>> show

b :: String -> String
b = lines >>> fmap readCmd >>> zipReversePaths >>> toFileMap >>> toDirSizes >>> filterBigOnes >>> minimum >>> show
    where
        filterBigOnes xs = let xmax = maximum xs in xs & filter ((xmax -) >>> (<= 70_000_000 - 30_000_000))

type FSize       = Int
type FName       = String
type ReversePath = [String]
data Cmd         = Cd !String | FileInfo !FName !FSize | Unimportant deriving (Show)

toDirSizes :: Map (FName, ReversePath) FSize -> [Int]
toDirSizes = toDirMap >>> Map.elems

toDirMap :: Map (FName, ReversePath) FSize -> Map ReversePath FSize
toDirMap = Map.assocs >>> fmap (\((_, path), x) -> (path, x)) >>> concatMap unroll >>> Map.fromListWith (+)

unroll :: (ReversePath, FSize) -> [(ReversePath, FSize)]
unroll (path, x) = fmap (, x) (tails path)

toFileMap :: [(Cmd, ReversePath)] -> Map (FName, ReversePath) FSize
toFileMap = mapMaybe toFileSize >>> Map.fromList

toFileSize :: (Cmd, ReversePath) -> Maybe ((FName, ReversePath), FSize)
toFileSize (FileInfo fname fsize, revpath) = Just ((fname, revpath), fsize)
toFileSize _                               = Nothing

zipReversePaths :: [Cmd] -> [(Cmd, ReversePath)]
zipReversePaths cmds = zip cmds (reversePaths cmds)

reversePaths :: [Cmd] -> [ReversePath]
reversePaths = scanl cd []

cd :: ReversePath -> Cmd -> ReversePath
cd cba (Cd "/")  = []
cd cba (Cd "..") = tail cba
cd cba (Cd d)    = d:cba
cd cba _         = cba

readCmd :: String -> Cmd
readCmd s
    | Just str            <- readCd s    = Cd str
    | Just (fname, fsize) <- readFile' s = FileInfo fname fsize
    | otherwise                          = Unimportant

readCd :: String -> Maybe String
readCd = stripPrefix "$ cd "

readFile' :: String -> Maybe (FName, FSize)
readFile' s
    | [fsize', fname] <- words s, all isDigit fsize' = Just (fname, read fsize')
    | otherwise                                      = Nothing

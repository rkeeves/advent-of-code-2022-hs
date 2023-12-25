import           Control.Arrow (first, second, (&&&), (***), (>>>))
import           Data.Function ((&))
import           Data.IntMap   (IntMap, (!))
import qualified Data.IntMap   as IntMap
import           Data.List     (foldl', transpose, unfoldr)

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> twoSepBy emptyLine >>> (readStacks *** readCmds) >>> craneWith reverse >>> stackHeads >>> show

b :: String -> String
b = lines >>> twoSepBy emptyLine >>> (readStacks *** readCmds) >>> craneWith id >>> stackHeads >>> show

type StackId = Int
data Cmd     = Cmd { n :: !Int, src :: !StackId, dst :: !StackId } deriving (Show)
type Stack   = String

stackHeads :: IntMap Stack -> String
stackHeads = IntMap.elems >>> fmap head

craneWith :: ([Char] -> [Char]) -> (IntMap Stack, [Cmd]) -> IntMap Stack
craneWith reorder (st, cmds) = foldl' (craneOnce reorder) st cmds

craneOnce :: ([Char] -> [Char]) -> IntMap Stack -> Cmd -> IntMap Stack
craneOnce rev sts (Cmd n src dst) = let (dst', src') = splitAt n (sts ! src) in sts & (IntMap.adjust (rev dst' ++) dst >>> IntMap.insert src src')

readStacks :: [String] -> IntMap Stack
readStacks = fmap (chunksOf 4 >>> fmap chunkCenter) >>> transpose >>> fmap (filter (/= ' ')) >>> zip [1 ..] >>> IntMap.fromList

chunkCenter :: String -> Char
chunkCenter ('[': x :']':_) = x
chunkCenter _               = ' '

readCmds :: [String] -> [Cmd]
readCmds = fmap (words >>> (\[_, n, _, src, _, dst] -> Cmd (read n) (read src) (read dst)))

twoSepBy :: Eq a => a -> [a] -> ([a], [a])
twoSepBy sep = span (/= sep) >>> (init *** drop 1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if n < 0 || null xs then Nothing else Just (splitAt n xs))

emptyLine :: String
emptyLine = ""

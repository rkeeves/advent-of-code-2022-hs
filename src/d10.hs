import           Control.Arrow ((>>>))

main :: IO ()
main = interact $ (\x -> ["A", a x, "B", b x]) >>> unlines

a :: String -> String
a = lines >>> readCmds >>> xsignals 1 >>> zip [1..] >>> filter isSample >>> fmap signalStrength >>> sum >>> show
    where
        signalStrength (i, x) = i * x
        isSample (i, _) = (i - 20) `mod` 40 == 0

b :: String -> String
b = lines >>> readCmds >>> xsignals 1 >>> zip [0..] >>> fmap paint >>> chunks 40 >>> unlines
    where
        paint (i, x) = if abs (x - (i `mod` 40)) < 2 then '#' else '.'

xsignals :: Int -> [Int -> Int] -> [Int]
xsignals x0 = scanl (flip ($)) x0 >>> init

readCmds :: [String] -> [Int -> Int]
readCmds = concatMap (words >>> readCmd)

readCmd :: [String] -> [Int -> Int]
readCmd ["noop"]    = [id]
readCmd ["addx", x] = [id, (+ read x)]

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = let (chunk, rest) = splitAt n xs in chunk : chunks n rest

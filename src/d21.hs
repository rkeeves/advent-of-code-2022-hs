import           Control.Arrow   ((>>>))
import           Data.Complex    (Complex ((:+)), realPart)
import           Data.List       (uncons)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

main :: IO ()
main = interact $ lines >>> readTokenMap >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: Map Name [String] -> String
a = unfoldGrammarOf "root" >>> foldGrammar node leaf >>> show
    where
        leaf :: Literal -> Int
        leaf     = read
        node :: Op -> Int -> Int -> Int
        node "+" = (+)
        node "-" = (-)
        node "/" = div
        node "*" = (*)

b :: Map Name [String] -> String
b = Map.adjust (\[l, _ ,r] -> [l, "==", r]) "root" >>> Map.insert "humn" ["?"] >>> unfoldGrammarOf "root" >>> foldGrammar node leaf >>> realPart >>> round >>> show
    where
        leaf :: Literal -> Complex Double
        leaf "?" = 0 :+ 1
        leaf s   = read s :+ 0
        node :: Op -> Complex Double -> Complex Double -> Complex Double
        node "+"  = (+)
        node "-"  = (-)
        node "/"  = (/)
        node "*"  = (*)
        node "==" = solveEq
        solveEq (a :+ i) (b :+ j) = ((b - a) / (i - j)) :+ 0

type Name    = String
type Token   = String
type Literal = String
type Op      = String
data Grammar = Node !Op !Grammar !Grammar | Leaf !Literal

unfoldGrammarOf :: Name -> Map Name [Token] -> Grammar
unfoldGrammarOf = flip unfoldGrammar

unfoldGrammar :: Map Name [Token] -> Name -> Grammar
unfoldGrammar m name = case m Map.! name of
    [x]        -> Leaf x
    [l, op, r] -> Node op (unfoldGrammar m l) (unfoldGrammar m r)

foldGrammar :: (Op -> a -> a -> a) -> (Literal -> a) -> Grammar -> a
foldGrammar node leaf (Leaf x)      = leaf x
foldGrammar node leaf (Node op l r) = node op (foldGrammar node leaf l) (foldGrammar node leaf r)

readTokenMap :: [String] -> Map Name [Token]
readTokenMap = mapMaybe (filter (/= ':') >>> words >>> uncons) >>> Map.fromList

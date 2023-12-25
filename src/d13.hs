import           Control.Applicative (Alternative (..),
                                      Applicative (pure, (*>), (<*), (<*>)),
                                      (<$), (<$>))
import           Control.Arrow       ((>>>))
import           Control.Monad       (Functor (fmap), Monad (return, (>>=)),
                                      MonadFail (..))
import           Data.Char           (isDigit)
import           Data.Functor        (Functor (..), (<$>))
import           Data.List           (sort, unfoldr)
import           Data.Maybe          (mapMaybe)

main :: IO ()
main = interact $ lines >>> mapMaybe (runParser rose >>> fmap fst) >>> (\x -> ["A", a x, "B", b x]) >>> unlines

a :: [Rose] -> String
a = pairs >>> fmap (uncurry compare) >>> zip [1..] >>> filter (snd >>> (< GT)) >>> fmap fst >>> sum >>> show

b :: [Rose] -> String
b = ([beg, end] ++) >>> sort >>> zip [1..] >>> filter (snd >>> (`elem` [beg, end])) >>> fmap fst >>> product >>> show
    where
        beg = Node [Node [Leaf 2]]
        end = Node [Node [Leaf 6]]

pairs :: [a] -> [(a, a)]
pairs = unfoldr pair
    where
        pair (x:y:xs) = Just ((x,y), xs)
        pair _        = Nothing

data Rose = Node ![Rose] | Leaf !Int deriving (Eq, Show)

instance Ord Rose where
    compare :: Rose -> Rose -> Ordering
    compare (Leaf x)  (Leaf x')  = compare x x'
    compare (Node xs) (Node xs') = compare xs xs'
    compare r@(Leaf x) r'        = compare (Node [r]) r'
    compare r@(Node x) r'        = compare r (Node [r'])

rose :: Parser Rose
rose = node <|> leaf

leaf :: Parser Rose
leaf = Leaf . read <$> some (char isDigit)

node :: Parser Rose
node = Node <$ char' '[' <*> sepBy (char' ',') rose <* char' ']'

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep token = (:) <$> token <*> many (sep *> token) <|> pure []

char' :: Char -> Parser Char
char' c = char (== c)

char :: (Char -> Bool) -> Parser Char
char p = Parser f
    where
        f []     = Nothing
        f (x:xs) = if p x then Just (x, xs) else Nothing

type ParserT a = String -> Maybe (a, String)

newtype Parser a = Parser { runParser :: ParserT a }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser pa) = Parser $ \s -> do
        (a, s') <- pa s
        return (f a, s')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \s -> Just (a, s)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser pa) = Parser $ \s -> do
        (f, s')  <- pf s
        (a, s'') <- pa s'
        return (f a, s'')

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser pa) >>= fpb = Parser $ \s -> do
        (a, s') <- pa s
        runParser (fpb a) s'

instance MonadFail Parser where
    fail :: String -> Parser a
    fail _ = Parser $ const Nothing

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser pa) <|> (Parser pa') = Parser $ \s -> pa s <|> pa' s

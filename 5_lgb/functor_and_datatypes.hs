import Data.List
import Data.Ord

--defining new data type
data Bool = False | True -- value on the left define new Value constructors

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

--value constructors as functions
val1 = map (Circle 10 20) [1,2,3,4]

-- Type parameters
data Maybe2 a = Nothing2 | Just2 a
    deriving (Show) -- a is a type parameter and Maybe2 is a type constructor


-- Functor -- Things which can be mapped over
--fmap takes a function from one type to another and a functor value applied with one type and returns a functor value applied with another type
class Functor2 f where
    fmap2 :: (a -> b) -> f a -> f b -- NOTE THE FUNCTION FMAP

instance Functor2 [] where
    fmap2 = map

instance Functor2 Maybe2 where
    fmap2 f (Just2 x)   = Just2 (f x)
    fmap2 f Nothing2    = Nothing2

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

main :: IO()
main = do
    print "Hello"
    print $ val1
    
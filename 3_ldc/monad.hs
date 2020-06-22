-- Monad, just like a function and a applicative, is a typeclass
-- Monad applies a function (regular fxn's like (+3) (*2))to a wrapped value and returns a wrapped value

-- Difference b/w this and functor is:
    -- fmap f (Just a) = Just (f a)
    -- in monad it will be using >>=
        -- we can do something like (Just 4) >>= half
        -- if half x was something like half (Just x) we would have gotten half (Just 10) => Just 5
            -- but that would need us changing the function declaration everytime
        -- instead >>= is used which 

-- so below half 10 will give answer 5 but half (Just 10) will not work
half x = if even x
    then Just (x `div` 2)
    else Nothing

x = Just 4 >>= half >>= half -- chaining
x1 = Just 4 >>= half >>= half >>= half

{--
class Monad m where
    --bind
    (>>=) :: m a -> (a -> m b) -> m b -- m is a monad.
    -- "m" = monad
    -- m a = (Just2 4)
    -- (a -> m b) = half this is a function which takes a simple value as a
--}
data Maybe2 a = Just2 a | Nothing2 deriving Show
instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2 = Nothing2

instance Applicative Maybe2 where
    pure = Just2
    Just2 f <*> (Just2 j) = Just2 (f j)
    --or written as
    Just2 f <*> j = fmap f j -- Run this: fmap (+3) (Just2 1)
    Nothing2 <*> j = Nothing2

instance Monad Maybe2 where
    Nothing2 >>= f = Nothing2
    Just2 val >>= f = f val -- automatically put inside a Just2

--Tree Tips
data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap func (Tip a) = Tip (func a)
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right)
    
instance Applicative Tree where
    pure = Tip
    Tip f <*> t = fmap f t
    Branch left right <*> t = Branch (left <*> t) (right <*> t)

g x | x == 4 = (Tip 99) | otherwise = Branch (Tip (x * 2)) (Tip (x * 3))

instance Monad Tree where
    Tip a >>= f = f a 
    Branch left right >>= f = Branch (left >>= f) (right >>= f)

x3 = Branch (Tip 4) (Branch (Tip 5) (Tip 6))


main :: IO ()
main = do
    print x
    print x1

    print x3
    print $ g 4
    print $ g 12

    print $ x3 >>= g
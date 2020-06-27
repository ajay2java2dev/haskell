{--
-- Applicative is a Type class and like a Interface again
-- They allow us to wrap FUNCTIONS and not just a Value. 
    -- For example : Just (+1) <*> (Just 8) which gives output in wrapped format Just 9
    -- List are examples of applicatives
-- Applicate functor signature
class (Functor f) => Applicative f where
    pure a :: a -> f a -- takes a value and put it into a container
    f (a -> b) <*> f a :: f b -- takes two containers f (a -> b) and f a and returns a container f b

-- <*> lifts the function application

    import Control.Applicative
    data Foo a = Foo a

    instance Show a => Show (Foo a) where
        show (Foo a) = "Foo " ++ show a

    instance Functor Foo where
        fmap f (Foo a) 

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

-- Samples :
{--
 
*Main> (*) <$> Just 8 <*> Just 2
Just 16
*Main> [(*2), (+3)] <*> [1,2,3]
[2,4,6,4,5,6]

 --}

data Tree a = Node a | Branch (Tree a) (Tree a) deriving Show
{--
*Main> :t Node
Node :: a -> Tree a
*Main> :t Branch
Branch :: Tree a -> Tree a -> Tree a
--}

{--
*Main> :t xBranch 
xBranch :: Tree Integer
--}
instance Functor Tree where
    fmap func (Node a) = Node (func a)
    -- here simply we cannot do Branch (func left) (func right) because we want the func to applied to the leaf nodes or to node
    -- so we have to apply the fmap recursively again to the func returns
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right)
    -- can also be written as Infix notations below:
    -- fmap func (Branch left right) = Branch (func <$> left) (func <$> right)

instance Applicative Tree where
    pure = Node
    Node f <*> t = fmap f t
    Branch left right <*> t = Branch (left <*> t) (right <*> t)

xBranch = Branch (Node 4) (Branch (Node 3) (Node 5))
--Below is how the tips get applied with the values. The operator-value (*4) gets applied on Node 4, 3 and 5
{--
*Main> Node (*4) <*> xBranch
Branch (Node 16) (Branch (Node 12) (Node 20))

*Main> Branch (Node (*4)) (Node (+3)) <*> xBranch
Branch (Branch (Node 16) (Branch (Node 12) (Node 20))) (Branch (Node 7) (Branch (Node 6) (Node 8)))
--}


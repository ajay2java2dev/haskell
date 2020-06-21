-- Reference : https://www.youtube.com/watch?v=xCut-QT2cpI&t=1s

-- A functor is a Type class which can be thought of an Interface
-- functor gives us a signature for mapping. We are trying here to map a operator over a function using a mapping kind of a function
-- fmap gives us the capability to map
-- haskel gives functors for Maybe, Either, Lists

--class Functor f where
--    fmap :: (a -> b) -> f a -> f b -- this means that gives a function (a -> b) apply the function f to a and return the function applied on b

-- unwrapped value : Value on its own. For example : (+3) 9 where 9 is unwrapped value
-- wrapped value : surrounded by something. For example : Maybe. (+3) Just 9 simply wont work since 9 is still wrapped

data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2 = Nothing2 

--infix notation <$>
x = (+3) <$> [1,2,3]

-- Applying functors to your own DS -- fmap will be applied at the leaf/tip nodes
data Tree a = Node a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap func (Node a) = Node (func a)
    -- here simply we cannot do Branch (func left) (func right) because we want the func to applied to the leaf nodes or to node
    -- so we have to apply the fmap recursively again to the func returns
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right)
    -- can also be written as Infix notations below:
    -- fmap func (Branch left right) = Branch (func <$> left) (func <$> right)

xTree = Branch (Node 4) (Branch (Node 3) (Node 6))

main :: IO ()
main = do
    print $ fmap (+3) (Just2 9)
    print $ fmap (+3) [1,2,3] -- is same as
    print $ fmap (+3) [1,2,3] -- this

    putStrLn "The above can also be rewritten with <$>"

    print $ (+3) <$> [1,2,3]
    print x

    print $ fmap (+3) xTree
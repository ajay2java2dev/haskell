
{--
Reference : https://www.youtube.com/watch?v=xCut-QT2cpI&t=1s

-- 1. A functor is a Type class which can be thought of an Interface. These are types whose values can be mapped over.
-- 2. functor gives us a signature for mapping. We are trying here to map a operator over a function using a mapping kind of a function
-- 3. fmap gives us the capability to map. 
        -- fmap:: (a -> b) -> f a -> f b . This means give me a fxn that takes an 'a' and returns a 'b' 
            and a box/container with an 'a' (or several of them) inside it and in return give a box/container with a 'b' (or serveral of them) inside it.
        -- this basically applies the function to the element inside the box
-- 4. haskel gives functors for Maybe, Either, Lists
    -- fmap applies a function to the value while preserving its context. Context is basically the conditions including Failure or empty.
        
    --Functor takes only type constructors of kind * -> *, which means exactly one concrete type as a type parameter. Example Maybe Int or Maybe String
    -- but in case of Either its * -> * -> *, which means it takes 2 type parameters and to reduce we have to partially apply it.
        -- like this :- instance Functor (Either a) where
            - additional implementation details below

-- 5. unwrapped value : Value on its own. For example : (+3) 9 where 9 is unwrapped value
-- 6. wrapped value : surrounded by something. For example : Maybe. (+3) Just 9 simply wont work since 9 is still wrapped

use below to define a map for many different types and the f-type you pass is must be a parametrized type

--this is a type class
class Functor f where
    fmap :: (a -> b) -> f a -> f b -- this means that gives a function (a -> b) apply the function f to a and return the function applied on b

--instance of the type class
instance Functor Maybe where
    fmap func (Just x) = Just (func x)
    fmap _ Nothing = f x : fmap f xs

--instance of the type class
instance Functor [] where
    fmap func [] = []
    fmap func (x:xs) = func x : fmap func xs

-- antoher class
class Functor f1 where
    fmap :: (b -> c) -> Either a b -> Either a c

--}



-- But lets try something of our own.
data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2 = Nothing2


--infix notation <$>
x = (+3) <$> [1,2,3]
y = [(+1),(*100),(*5)] <*> [1,2,3]

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



--Either with functor (http://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.Either.html#Either)

data Either1 a b = Left1 a | Right1 b deriving (Eq, Ord, Read, Show)
instance Functor (Either1 a) where
    fmap _ (Left1 x) = Left1 x
    fmap func (Right1 y) = Right1 (func y)


-- IO Actions : https://hackage.haskell.org/package/ghc-prim-0.6.1/docs/src/GHC.Types.html#IO
{--
newtype IO2 a = IO2 (State# RealWorld -> (# State# RealWorld, a #))
type role IO representational
instance Functor IO2 where
    fmap f action = do
        result <- action
        return (f result)
--}

main :: IO ()
main = do
    print $ fmap (+3) (Just2 9)
    print $ fmap (+3) [1,2,3] -- is same as
    print $ fmap (+3) [1,2,3] -- this

    putStrLn "The above can also be rewritten with <$>"

    print $ (+3) <$> [1,2,3]
    print x
    print $ y
    
    print $ fmap (+3) xTree
    
    putStrLn $ "Please enter your name : "
    line <- fmap reverse getLine
    putStrLn $ "Did you say ?? : " ++ line
    
import Data.List
import Data.Ord

-- This contains just some samples of using dot operator: 
-- 1. https://medium.com/@zarinfam/what-is-the-dot-operator-in-haskell-74b92a4d94e9

-- I always wonder why in haskell, the functions are too small. Was it justfied to be so small ? 

--1. simple pow fxn
pow2 :: Integer -> Integer
pow2 a = a * a

--2. adding additional functionality
isPow2Even :: Integer -> Bool
isPow2Even x = even $ pow2 x

--3. Above looks great and elegant. Create a function and pass result to another function. Some kind of automatic chaining.
-- but lets simplify it.
isPow2ReallyEven :: Integer -> Bool
isPow2ReallyEven = even . pow2 -- looks like a f(g(x)) where (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- result of . operator is another function (or lambda) that you can use and call. Compose can help in chaining.


-- more examples
--2. list of integers and sort it
desort :: [Integer] -> [Integer]
desort = reverse . sort -- its cleaner but then you have to keep in mind how the values are internally getting passed over and how lambdas are getting constructed

main :: IO()
main = do
    print "https://medium.com/@zarinfam/what-is-the-dot-operator-in-haskell-74b92a4d94e9"

    print $ isPow2ReallyEven 10
    print $ desort [12,11,14,15]

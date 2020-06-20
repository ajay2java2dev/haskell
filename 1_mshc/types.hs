import Data.Complex
import Data.Ratio
import Data.List

-- by Richard Cook


-- ALGEBRAIC DATA TYPES

-- NOTES: Types dont introduce new types. It just used to create Synonyms

-- 1. Type Synonyms
-- 2. Type Signature for values
-- 3. Type Signature for functions
-- 4. Polymorphism
-- 5. Constraints

type Port = Int -- This makes Port a synonym for Int type
type HostInfo = (String, Port) -- tuple

--examples
ipaddress :: HostInfo
ipaddress = ("mycomputer.com", 8080) --remember the double quotes

n0 :: Int
n0 = 5

n1 :: Double
n1 = 5.0

n2 :: Complex Double
n2 = 2 :+ 3 -- :+ is a Data constructor to create a complex number

n3 :: Ratio Int -- ratio of Int
n3 = 2 % 3

-- char literals
c0 :: Char
c0 = 'X'

c1 :: Char
c1 = '\0088' -- decimal unicode syntax

c2 :: Char
c2 = '\x0058' -- hexadecimal unicode format

c3 :: Char -- octal unicode syntax
c3 = '\o0130'

-- Strings
s0 :: String
s0 = "abc"

s1 :: String
s1 = "\0088\x0058\o0130"

-- list 
l0 :: [Int]
l0 = [1,2,3,4,5]

l1 :: [Int]
l1 = [1..10]

l2 :: [Int]
l2 = [1,3 .. 10]

l3 :: [Int]
l3 = [1..]

l4 :: [String]
l4 = ["aaa","bbb","ccc","ddd"]

l5 :: [Char]
l5 = ['a' .. 'e']

l6 :: [Char]
l6 = "Hello"

l7 = "Hello" -- this is also [Char] but in String literal format.

-- Tuples
t0 :: (Int, Int)
t0 = (1234, 5678)

t1 :: (String, Int, Double) -- hetrogenous
t1 = ("sometext", 123, 3.14141)

t2 :: ([Int],[String], [(Float, Char)])
t2 = ([1,2,3,4,5],["Hello"],[(95.5,'C'),(96.5,'C'),(66,'F')]) -- nested type


-- Functions and not simple types
f0 :: String -> Int
f0 = length

f1 :: String -> (String, Int) -- original string and its length is returned as a tuple
f1 x = (x, length x)

f2 :: [String] -> [(String, Int)]
f2 = map f1

-- more than one arrow
formatList :: String -> String -> String -> [String] -> String
formatList start end seperator xs = start ++ (intercalate seperator (map show xs)) ++ end

-- below is just a type signature representation which demonstrates 
-- that every function in HASKELL we can only pass Single Argument and returns a value
formatList1 :: (String -> (String -> (String -> ([String] -> String))))
formatList1 start end seperator xs = start ++ (intercalate seperator (map show xs)) ++ end


--polymorphism in haskell with recursive fxns
-- : colon operator is the CONS operator for constructing lists
-- Haskell generates mono-morphic versions of these at compile time

-- Example 1
myMap :: (a -> b) -> [a] -> [b] -- takes a function from a to b, input as array of a and returns array of b
myMap _ [] = [] -- return an empty for cases where there a function given but the input array is empty

-- I/P : here f => represents a function (could be any function) and (a : as) represent an list for [a] input
-- O/P : here output is that function applied on the first element of the array. Then call myMap recursively passing the function and rest of the arguments
-- Note : f = fxn can be something like show. Even a custom function which takes input as an array and returns an array
myMap f (a : as) = f a : myMap f as 


-- Example 2
myFilter :: (a -> Bool) -> [a] -> [a] -- take a predicate function and an array as input and return the array as filtered
myFilter _ [] = []
-- if the function applied on first value of list is true, 
    -- then return the element along with a recursive call on myFilter with its function and rest of list as arguments
    -- else just make a recursive call with the function and rest of list as argument. Consider this step as filtering the list
myFilter f (a : as) = if f a then a : myFilter f as else myFilter f as

-- Example 3
myFold :: (a -> b -> b) -> b -> [a] -> b -- takes an aggregration function, an input b and another array input [a] and returns a value b
myFold _ b [] = b
-- f a b -- applies function on first value of the array and the argument b
-- myfold f (f a b) as -- applies the function on the result of (f a b) and rest of the elements in the arrays
myFold f b (a : as) = myFold f (f a b) as

-- mySum :: _ -- helps to determine the type signature. _ is a type hole and represents a wildcard
mySum :: Num a => [a] -> a -- the (+) creates a constraint of type Num and hence Num a is added here
mySum = myFold (+) 0

main :: IO ()
main = do
    print ipaddress
    print n0
    print n1
    print n2
    print n3

    print c0
    putChar c1 >> putChar '\n' -- note its single quotes here
    putChar c2 >> putChar '\n'
    putChar c3 >> putChar '\n'

    putStrLn s0
    putStrLn s1

    print l0
    print l1
    print l2
    print (take 20 l3)
    print l4
    print l5
    print l6
    print l7
    
    print t0
    print t1
    print t2

    -- Since FUNCTION's cannot be called directly, it has to be given like this as a result of some application
    print $ f0 "Hello"
    print $ f1 "Hello"
    print $ f2 ["Hello", "Testing"]

    putStrLn $ formatList "<List>" "</List>" "|" ["first", "second", "third", "fourth"]

    print $ myMap show [10,20,30] -- show the function
    print $ myFilter (<25) [10,20,30] -- <25 the predicate function
    print $ myFold (+) 100 [10,20,30] -- (+) the function

    print $ mySum [10,20,30]

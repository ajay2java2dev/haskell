import Data.Complex
import Data.Ratio

-- by Richard Cook


-- ALGEBRAIC DATA TYPES

-- NOTES: Types dont introduce new types. It just used to create Synonyms

-- 1. Type Synonyms
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
t2 = ([1,2,3,4,5],["Hello"],[(95.5,'C'),(96.5,'C'),(66,'F')])

-- 2. Type Signature for values


-- 3. Type Signature for functions


-- 4. Polymorphism


-- 5. Constraints

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
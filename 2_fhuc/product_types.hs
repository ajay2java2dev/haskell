{-# LANGUAGE FlexibleContexts #-}
-- product types include tuples and records as shown below  

module ProductType where

import Data.Complex
import Data.Ratio
import Data.List
import Data.Map

xtup = 10
ytup = "Hi"
ptup = (xtup,ytup) -- pair
ftup = fst ptup
stup = snd ptup
pdtup = (xtup, ytup, (\x -> x + 1), (2,3))


-- The below code is an example of how the complex numbers can be represented as tuples and operations completed.
cadd (a,b) (c,d) = (a+c,b+d)
--  :t cadd
--  cadd :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
cmul (a,b) (c,d) = ((a*c - b*d), (a*d + b*c))
-- :t cmul
-- cmul :: Num b => (b, b) -> (b, b) -> (b, b)

-- but the above code is kind of not efficient since it covers all kinds of complex numbers.
-- HENCE WE USE RECORD PRODUCT TYPE to exactly specify to the compiler what we need
data Complex = Complex {
    re :: Float, -- NOTE: THIS FIELD IS ALSO A FUNCTION INTERNALLY
    im :: Float --NOTE :  THIS FIELD IS ALSO A FUNCTION INTERNALLY
} deriving (Show , Eq)
-- since the fields are functions they have to unique within the same scope

c1 = Complex 10.54 43.2 -- prints : Complex {re = 10.54, im = 43.2}
c2 = Complex {re = 10.54, im = 32.1} -- prints : Complex {re = 10.54, im = 32.1}

-- Database records
data Person = Person {
    fname   :: String,
    lname   :: String,
    age     :: Int
} deriving (Show , Eq, Ord)
people = [Person "Ajay" "Menon" 34, Person "Reshmi" "Ravi" 30]

myPeopleLookup :: (Ord k) => k -> [Person] -> Maybe Person
myPeopleLookup _ [] = Nothing
-- TODO : create a new associative list where instead of tuples you have a record type

-- Write a associate list. Create a function add which take a key and a value and returns a list/map with the key and value inserted.
-- Make the same key sorted.

-- here simple associative list
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]

--TODO : write a function mylookup which takes the key and a associative list / a map and returns the correspoding value
-- => using this notation we say that the key should be comparable.
-- i/p is a key and list with paired values and return is a new value.
mylookup :: (Eq k) => k -> [(k,v)] -> v
mylookup key xs = snd . head . Prelude.filter (\(k,v) -> key == k) $ xs

-- the above will crash when no data so we improve with Maybe
mylookup2 :: (Eq k) => k -> [(k,v)] -> Maybe v
mylookup2 _ [] = Nothing
mylookup2 key ((k,v):xs) = if   key == k
                                then Just v
                                else mylookup2 key xs

myMappedList = fromList phoneBook
myNewMappedList = Data.Map.insert "Ajay" "804-655" myMappedList


main :: IO()
main = do
    print $ ftup
    print $ stup

    print $ cadd (1,2) (3,4)
    print c1
    print c2
    print $ re c1 -- wow this is niceeee. re of c1 and hence the scope of re with c1 and nowhere else. 
    print $ im c2

    print $ people
    print $ mylookup "penny" phoneBook
    --print $ mylookup "Ajay" phoneBook -- will fail
    print $ mylookup2 "ajay" phoneBook
    print $ mylookup2 "penny" phoneBook

    print $ myMappedList
    print $ myNewMappedList
    
    
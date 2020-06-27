  -- product types include tuples and records as shown below  

module ProductType where

import Data.Complex
import Data.Ratio
import Data.List

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




main :: IO()
main = do
    print $ ftup
    print $ stup

    print $ cadd (1,2) (3,4)
    print c1
    print c2
    print $ re c1 -- wow this is niceeee
    print $ im c2 -- wow this is niceeee TOOO
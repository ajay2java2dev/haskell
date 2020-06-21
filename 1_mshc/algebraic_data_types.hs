
module SimpleColorumType (Color) where
    

import Data.Complex
import Data.Ratio
import Data.List

-- Color : This is Type Constructor
-- Red, Gree or Blue are Data Constructor
-- Hence Color (type) cannot be assigned to something. Only Data constructors can be used for assignment
data Color = Red | Green | Blue deriving Show -- pipe | used to signify the data constructors are alternatives

data RGBColor = RGB {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving Show

-- x = Color -- Will throw error Data constructor not in scope
r = Red -- r:: Color
g = Green -- r:: Color
b = Blue -- r:: Color

data MyBool = False | True deriving Show

data Quaternion = Q
    {
        qR :: Double,
        qI :: Double,
        qJ :: Double,
        qK :: Double
    } -- deriving Show -- without deriving show the print on this will throw error : No instance of Show on Quaternion found ...

-- using deriving Show we would get an O/P : Q {qR = 1.0, qI = 2.0, qJ = 3.0, qK = 4.0}
-- using instance Show Quaternion we would get O/P as : (1.0 + 2.0i + 3.0j + 4.0k)
instance Show Quaternion where
    show q = "(" ++
        show (qR q) ++ " + " ++ 
        show (qI q) ++ "i + " ++ 
        show (qJ q) ++ "j + " ++ 
        show (qK q) ++ "k)"

instance Num Quaternion where
    q0 + q1 = Q (qR q0 + qR q1) (qI q0 + qI q1) (qJ q0 + qJ q1) (qK q0 + qK q1)
    q0 * q1 = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined


newtype PrettyQuaternion = PrettyQuaternion {unPrettyQuaternion :: Quaternion}
newtype UglyQuaternion = UglyQuaternion {unUglyQuaternion :: Quaternion}

instance Show PrettyQuaternion where
    show q = let q' = unPrettyQuaternion q in "(" ++
        show (qR q') ++ " + " ++ 
        show (qI q') ++ "i + " ++ 
        show (qJ q') ++ "j + " ++ 
        show (qK q') ++ "k)"

instance Show UglyQuaternion where
    show q = let q' = unUglyQuaternion q in "(" ++
        show (qR q') ++ " + " ++ 
        show (qI q') ++ " + " ++ 
        show (qJ q') ++ " + " ++ 
        show (qK q') ++ ")"


-- EQ Class
data Foo = Foo Int
x = Foo 10
y = Foo 10

instance Eq Foo where
    (==) (Foo i) (Foo j) = i == j

data Foo1 = Foo1 Int deriving Eq
x1 = Foo 10
y1 = Foo 20

class (Eq a) => Ord a where
    compare                 ::      a -> a -> Ordering
    (<), (<=), (>), (>=)    ::      a -> a -> Bool
    max, min                ::      a -> a -> a

    compare x y =   if x == y then EQ
                    else if x SimpleColorumType.<= y then LT
                    else GT

    x < y   = case SimpleColorumType.compare x y of { LT -> Prelude.True; _ -> Prelude.False}
    x <= y  = case SimpleColorumType.compare x y of { GT -> Prelude.False; _ -> Prelude.True}
    x > y   = case SimpleColorumType.compare x y of { GT -> Prelude.True; _ -> Prelude.False}
    x >= y   = case SimpleColorumType.compare x y of { LT -> Prelude.True; _ -> Prelude.False}

    max x y = if x SimpleColorumType.<= y then y else x

main :: IO ()
main = do

    print $ (x == y)
    print $ (x1==y1)

    putChar '\n'

    let x = RGB 10 20 21
    print x

    print $ RGB 10 20 30

    print $ Q 1 2 3 4
    print $ Q 1 2 3 4 + Q 10 11 12 13

    let x1 = (PrettyQuaternion (Q 1 2 3 4))
    print $ "Pretty >>>>> " ++ show x1
    let x2 = (UglyQuaternion (Q 1 2 3 4)) 
    print $ "UnPretty >>>>> " ++ show x2
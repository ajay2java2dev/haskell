{-# LANGUAGE FlexibleContexts #-}
-- product types include tuples and records as shown below  

module ProductType where

import Data.Complex
import Data.Ratio
import Data.List
import Data.Map

-- recursive datatypes...yeahhh fun
data List a = Cons a (List a) | Nil deriving Show
insertSorted a Nil = Cons a Nil
insertSorted a (Cons b bs)
    | a < b     = Cons a (Cons b bs)
    | otherwise = Cons b (insertSorted a bs)

l1 = insertSorted 3 (Cons 2 (Cons 4 Nil))
x1 = Cons 1 (Cons 2 (Cons 3 Nil)) -- List of Integers
x2 = Cons "Hi" (Cons "there" (Cons "Ajay" Nil)) -- Lost of Strings
x3 = Cons Nil (Cons (Cons 5 Nil) Nil) --List of Lists


--BST ADD
data Tree a = Node a (Tree a) (Tree a) | Empty
add_bst :: Integer -> Tree Integer -> Tree Integer
add_bst i Empty = Node i Empty Empty
add_bst i (Node x left right)
    | i <= x = Node x (add_bst i left) (right)
    | otherwise = Node x left (add_bst i right)

-- MAYBE DATATYPE
data Maybe2 a = Just2 a | Nothing2 deriving (Show, Eq, Ord)
getItem key [] = Nothing2
-- NOTE: The associate pair as shown below
getItem key ((k,v):xs) =
    if key == k then Just2 v
    else getItem key xs

list1 = [(123,"Ajay"),(321,"Reshmi")]
list2 = [("Ajay",123),("Reshmi",321)]


-- EITHER DATATYPE
data Either2 a b = Left2 a | Right2 b deriving (Show, Eq, Ord)
getItem1 key [] = Left2 "Key not found"
getItem1 key ((k,v): xs) = 
    if key == k then Right2 v
    else getItem1 key xs

list3 = [(123,"Ajay"),(321,"Reshmi")]
list4 = [("Ajay",123),("Reshmi",321)]
xp3 = getItem1 123 list3
xp4 = getItem1 "Ajay" list4


--TODO: Try add, find , lookup, delete on a tree
-- add :: Tree a -> a -> Tree a
-- find :: Tree a -> a -> Bool
-- lookup :: Tree (k,v) -> k -> Maybe v
-- delete :: Tree a -> a -> Tree a

main :: IO()
main = do
    print $ l1
    print $ x1
    print $ x2
    print $ x3

    print $ getItem 123 list1
    print $ getItem "Ajay" list2

    print xp3
    print xp4


--1.
inc x = x + 1
double x = x * 2
compose f g x = f (g x)

--2.
twice f x = f (f x)

--3. function in literal syntax
-- \x -> x + 1

main :: IO()
main = do

    print $ compose inc double 10

    print $ twice inc 10
    print $ twice twice inc 10

    print $ (\x -> x + 1) 41
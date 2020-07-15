-- simple recurssions
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: [Int] -> Int
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs

product' :: (Num a) => [a] -> a
product' [] = 0
product' (x:xs) = x * product' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum in an empty array not available"
maximum' [x] = x
maximum' (x:xs)
    | x > max = x
    | otherwise = max
    where max = maximum' xs

ntsq :: Int -> Int
ntsq 1 = 1
ntsq n = (2 * n-1) + ntsq (n-1)

foo a =
    let aa = a * a
    in aa + a

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * fact (n -1 )

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

--this is bit different. Each element takes the operation
incList :: (Num a) => [a] -> [a]
incList [] = []
incList (x:xs) = x+1 : incList xs -- incrementing within an array itself.

fun1 [] = 0
fun1 (x:xs)
    | even x = fun1 xs - 1
    | odd x = fun1 xs + 1

fun2 1 = 0
fun2 n = 1 + fun2 (n `div` 2)

fun3 1 = 0
fun3 2 = 1
fun3 n = fun3 (n-1) + fun3 (n-2)

-- tail recurssion
-- tail call is one where its pure call and the return value is not expected.
-- below we have aux(n-1) call which is a tail recurssion.
fact2 n = aux n 1
    where 
        aux 0 a = a
        aux n a = aux (n-1) (a*n)

sum''' xx = aux xx 0
    where
        aux [] a = a
        aux (x:xs) a = aux xs (x + a)

{--
fun1' xx = aux xx 0
    where
        aux [] a = a
        aux (x:xs) 
            | even x    = aux xs (a+1)
            | odd x     = aux xs (a-1)
--}

fun2' n = aux n 1
    where
        aux 0 a = a
        aux n a = aux (n `div` 2) (a+1)


fun3' n = aux n 1 1
    where
        aux 0 f1 f2 = f1
        aux n f1 f2 = aux (n-1) f2 (f1 + f2)


tailPower :: Integer -> Integer -> Integer
tailPower x y = tailPower' 1 x y
    where 
        tailPower' n x 0 = n
        tailPower' n x y
            | n `seq` x `seq` y `seq` False = undefined
            | even y = tailPower' n (x*x) (y `div` 2)
            | otherwise = tailPower' (n*x) x (y-1)


main :: IO ()
main = do
    print $ tailPower 10 20
    print $ 30 `seq` 10 `seq` 20 `seq` False


    
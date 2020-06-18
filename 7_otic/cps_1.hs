import Distribution.Simple
main = defaultMain

--these contains examples I have tried for continous functions

-- GCD OF A LIST

-- GCD - A Continuation Solution
-- 1. newk = aux fxn has its own continuation name = newk
-- 2. aux now runs on 2 different continuation in its scope. One is the current continuation newk and a gcdstart newk continuation
-- 3. base case aux calls newk with base case 0
-- 4. the usual continuation case (\res -> newk (gcd x res)) --> aux creates a new continuation by wrapping a lambda around a current continuation.
-- 5. the parameter res will get its results by recurrsive calls to aux. This is just a accumalator recurrsion.
-- 6. magic happens on line 3 when we find 1 in the list. Instead of calling newk we call top continuation k instead.
gcdstar xx k = aux xx k
    where   aux [] newk     =   newk 0
            aux (1:xs) newk =   k 1
            aux (x:xs) newk =   aux xs (\res -> newk (gcd x res))
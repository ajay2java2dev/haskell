import Distribution.Simple
main = defaultMain

--these contains examples I have tried for continous functions



-- GCD - A Continuation Solution
gcdstar xx k = aux xx k
    where   aux [] newk     =   newk 0
            aux (1:xs) newk =   k 1
            aux (x:xs) newk =   aux xs (\res -> newk (gcd x res))
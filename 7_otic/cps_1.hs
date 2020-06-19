import Distribution.Simple
main = defaultMain

-- HEY ! Functions written in CPS don't return, they simply pass the result to the functions


--Direct Style Samples
ddec a = a - 1
ddouble a = a * 2
dinc a = a + 1
dreport a = a

-- RUN : > ddec 3 => gives output 2

--Continuos pass style
cdec a k = k (a - 1)
cdouble a k = k (a * 2)
cinc a k = k (a + 1)
creport a = a

-- more changes
cinc1 a k = k (a+1)
cdec1 a k = k (a-1)
cadd1 a b k = k (a + b)
creport1 a = a

-- RUN : > cinc1 4 (\a -> cdec1 5 (\b -> cadd1 a b creport1))
-- => output : 9


-- ##### Continuations and Recursion #####
-- simple recurssion.
multlist []  = 1
multlist (x:xs) = x * (multlist xs)
{--
    i/p : multlist [2,3,4] , o/p : 24
    stacktrace:
    2 * (multlist [3,4])
    2 * (3 * (multlist [4]))
    2 * (3 * (4 * (multlist [])))
    2 * (3 * (4 * 1))
--}

-- Accumalator recurssion
amultlist [] acc        = acc
amultlist (x:xs) acc    = amultlist xs (x * acc)

--this has errors: TODO : FIXME
{--
kmultlist k []     = k 1
kmultlist k (x:xs)  = kmultlist xs (\r -> k (r * x))
--}

kmultlist xx k = aux xx k
    where   aux [] kr       = kr 1
            aux (0:xs) kr   = k 0
            aux (x:xs) kr = aux xs (\v -> kr (v *x))
{--
    i/p : multlist [2,3,4] creport, o/p : 24
--}



{--

NOTE: THESE STATEMENTS BELOW MAY SHOW IMPERATIVE STYLE OF CODING.

    : > cdec 3 creport => gives output 2
    : > cdec 3 (\r -> cdouble r creport) 
        => gives output 4. here note the cdec takes argument 3 and rest is the continuation part.
        => The resultig expression after executing cdec 3 gives output 2 to the continuation like this. (cdouble 2 creport)
        => A similiar thing happens and cdouble 2 gets executed, passing 4 as the continuation result to creport.

    => so if we have to include cinc to the function and make it in C-Style
    : > cdec 3 (\r1 -> cdouble r1 (\r2 -> cinc r2 creport)) --output is 5
    : > cinc 3 (\r2 -> cdouble r2 (\r1 -> cdec r1 creport)) --output is 7


--}

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


-- formal definition of CPS transform to do Transformations
{--
    1. Top level declaration : To convert a declaration, add a continous argument to it and then convert the body
        1.1 Create a new variable k
        1.2 Variable name need to be fresh and not already used in the function
        1.3 Notice that the k at the end is a subscript now
        
    C [[f arg = e]] => f arg k = C [[e]] k

    2. The second situation is transforming simple expressions.
        1.1 The expression is simple meaning there is no function that can be activated in the code
        1.2 Function is unavailable if its within a lambda expression & hence wont get called.
        1.3 A simple expression in tail position should be passed to a continuation instead of being returned

        C[[a]]k => k a

        - above the first k is subscript
        - "Simple" no available function calls
        - f a is available in 3 + f a, but not in lambda x.x + f a

--}

-- Examples CPS function convertion
{-- 

f x = x         becomes     f x k = k x

pi1 a b = a     becomes     pi1 a b k = k a

const x = 10    becomes     const x k = k 10

--}

-- Function call on Simple Argument.
-- C [[f arg = e]] => f arg k = C [[e]] k

-- Function call on Non Simple Argument.
-- C [[f arg]]k = C[[arg]] (\v.f v k)
inc x = x + 1

-- Try converting these functions.

foo1 0 = 0
foo1 n   | n < 0     = foo1 n
        | otherwise = inc (foo1 n)
{--
foo2 0 k    = 0
foo2 n k    | n < 0         = foo2 n k
            | otherwise     = foo2 n (\v -> inc v k)
--}


-- Cases with an operator
{--
    -- if 2 arguments are simple, then whole thing is simple

    C [[e1 + e2]]k  => k (e1 + e2)

    -- if one of the arguments is not simple
    
    C [[e1 + e2]]k  => c[[e1]] (\v -> k(v + e2)) where v is fresh

    -- if no arguments are simple

    C [[e1 + e2]]k => C[[e1]] ((\v1 -> C[[e2]] \v2 -> k(v1 + v2)) where v1 and v2 are fresh

--}

foo1_old a b = a + b
foo1_new a b k = k (a + b)

foo2_old a b    = inc a + b -- here a is complex and need to be converted
--foo2_new a b k  = inc a (\v -> k (v + b))

foo3_old a b    = a + inc b
--foo3_new a b k  = inc b (\v -> k (a + v))







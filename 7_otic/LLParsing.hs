-- NOTE: THESE ARE JUST NOTE <<<< THE CODE BELOW HAS ERROR AND SOME ARE JUST FOR DEMO >>>>
-- GRAMMAR (LL PARSING IN GRAMMARS)
import Distribution.Simple
main = defaultMain

--lecture 1.1 LL Parsing

getE [] = undefined
getE ('*':xs) =
    let e1, r1 = getE xs
        e2, r2 = getE r1
    in (ETimes e1 e1, r2)

-- what could possibly go wrong ... left recurssion
-- A basic rule like E -> E + E would cause infinite recurssion
-- If non-terminal appears on the right hand side, then the corresponding function is called before 
-- any input is consumed
getE xx = 
    let e1,r1 = getE xx
        ('+':r2)    = r1
        e2, r3      = getE r2
    in  (EPlus e1 e2, r3)

-- Common prefix issues like
E -> - E
    or -EE 
-- above would confuse a function. Which version should be used ??
getE ('-':xs) = ...  -- unary rule
getE ('-':xs) = ...  -- binary rule
-- something like these E -> xA and S -> xB wont qualify for common prefix. 
-- it has to be something like E -> E + E


-- lecture 1.2 fixing Non LL-Grammars to LL-Grammars. Do as shown below
{--
Note these:
Eg 1:
E -> E+
E -> i
which means we start with i but that follows as many + as required.

Solution:
E   -> iE`
E`  -> +E` | epsilon


Eg 2:
Another one:
B -> Bxy | Bz | q | r

which means we start with q or r and then follow by x y or z

Solution:
B   -> qB`  | rB`
B'  -> xyB` | zB` | epsilon


--}

--Question: BUT what we have lot of Left Recurssive grammars to make them LL
-- Lot of Left recurssion means MUTUAL RECURSSION. Apart from the obvs recurssion,
-- there would be another additional recurssion called the backward recurssion.

{--
A -> Aa | Bb | Cc | q
B -> Ax | By | Cz | rA
C -> Ai | Bj | Ck | sB
--}

{--
Solution:
Step 1 : Take the first symbol (A) and eliminate immediate left recurssion
A -> Aa | Bb | Cc | q
becomes
A  -> BbA` | CcA` | aA`
A` -> aA` | epsilon

Step 2 : Take the second symbol (B) and subsititute left recurssion to A. 
Then eliminate immediate left recurssion in B

B -> Ax | By | Cz | rA

now before we elimiate the obvious left recurssion B -> By,
we need to remove all the backward recurssion created here

so,
Start: B -> Ax (this represents the backedge which can cause mutual recurssion)
which can also be written as : B -> BbA'x | CcA`x | qA`x

So finally once the backedge is eliminated.
B -> BbA'x | CcA`x | qA`x | By | Cz | rA
and now we can remove the simple B recurssion as below
B -> CcA`xB` | qA`xB` | ByB` | CzB` | rAB`
B -> bA`xB`  | yB`    | epsilon

Step 3 : Take the 3rd Symbol (C) and sub left recurssions to A and B. Then eliminate
immediate left recurssion to C

C -> CcA`xB`bA`i | qA`xB`bA`i | ByB`bA`i | CzB`bA`i | rAB`bA`i
     CcA`xB`j | qA`xB`j | ByB`j | CzB`j | rAB`j
     CaA`i    | qA`i    | Ck    | sB


--}
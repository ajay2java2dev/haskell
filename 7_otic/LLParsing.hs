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
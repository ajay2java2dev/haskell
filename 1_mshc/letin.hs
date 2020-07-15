hyp a b = sqrt (a ^ 2 + b ^ 2)

identifyMaleOrFemale person = if person == "m"
                                then "male"
                                else "female"
-- HASKELL FUNCTION MUST ALWAYS RETURN A VALUE. WITHOUT ELSE THE function will be undefined.

-- LET IS USED TO DEFINE FUNCTIONS on THE FLY
-- let double x = 2 * x -- like this can be mentioned in Main function


main :: IO()
main = do
    print $ hyp 3 4
    print $ identifyMaleOrFemale "m"
    print $ identifyMaleOrFemale "f"
    
    
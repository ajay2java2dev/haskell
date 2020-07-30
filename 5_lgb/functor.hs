{--
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
--}

{--
main = do   line <- getLine
            let line' = reverse line
            putStrLn $ "You said " ++ line' ++ " backwards!"
            putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
--}

-- also can be written with a fmap
{--
main = do   line <- fmap reverse getLine
            putStrLn $ "You said " ++ line ++ " backwards!"
            putStrLn $ "Yes, you said " ++ line ++ " backwards!"
--}
import Data.Char
import Data.List

-- function composition 
main = do   line <- fmap (intersperse '-' . reverse . map toUpper) getLine
            putStrLn line

-- also can be written as
x = (\xs -> intersperse '-' (reverse (map toUpper xs)))


-- Control.Monad.Instances:
{--
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
--}

-- OR 
instance Functor ((->) q) where
    fmap = (.)

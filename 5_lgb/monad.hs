-- use :! clear to clear the terminal
-- :info Monad
{--

class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}
        -- Defined in ‘GHC.Base’
instance Monad (Either e) -- Defined in ‘Data.Either’
instance Monad [] -- Defined in ‘GHC.Base’
instance Monad Maybe -- Defined in ‘GHC.Base’
instance Monad IO -- Defined in ‘GHC.Base’
instance Monad ((->) r) -- Defined in ‘GHC.Base’
instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’

--}


-- sample code below from https://www.youtube.com/watch?v=IBB7JpbClo8&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=17

-- notes:
{--(>>=) is a bind operation and basically used to retrieve/extract the internal value --}
{--
    Just 1 >>= (\x -> Just x) gives ouput Just 1
    Nothing >>= (\x -> Just x) gives output Nothing
--}

maybeadd :: Num b => Maybe b -> b -> Maybe b
maybeadd mx y = mx >>= (\x -> Just $ x + y) -- this means extract from mx the value and add that with value passed in parameter y

--below for 2 monads
maybeadd_crazy :: Num b => Maybe b -> Maybe b -> Maybe b
maybeadd_crazy mx my = mx >>= (\x -> my >>= (\y -> Just $ x + y)) -- here it says extract value within 2 Maybes and add them together.

-- trying with return
maybeadd_crazy2 :: Num b => Maybe b -> Maybe b -> Maybe b
maybeadd_crazy2 mx my = mx >>= (\x -> my >>= (\y -> return $ x + y))

-- but even better do it like this such that this works on IO ins as well:
maybeadd_crazy3 :: (Monad m, Num b) => m b -> m b -> m b
maybeadd_crazy3 mx my = mx >>= (\x -> my >>= (\y -> return $ x + y))

-- rewritting monad using do-notation
monadd mx my = do
    x <- mx
    y <- my
    return $ x + y

-- Greater greater (>>) THE ANONYMOUS BIND

main :: IO()
main = do
    print $ "Welcome to Monads examples. >>= helps in getting the internal value"
    
    print $ "Simple monadic operation. Showcasing Binds"
    print $ Just 1 >>= (\x -> Just x)
    --print $ Nothing >>= (\x -> Just x)

    print $ "Introducing maybe add"
    print $ maybeadd Nothing 1
    print $ maybeadd (Just 1) 1

    print $ "Plumbing multiple monads"
    print $ maybeadd_crazy (Nothing) (Nothing)
    print $ maybeadd_crazy (Nothing) (Nothing)
    print $ maybeadd_crazy (Just 2) (Just 1)

    print $ "Plumbing multiple monads with return"
    print $ maybeadd_crazy2 (Nothing) (Nothing)
    print $ maybeadd_crazy2 (Nothing) (Nothing)
    print $ maybeadd_crazy2 (Just 2) (Just 1)
    
    print $ "Plumbing multiple monads with return and types constrained to Monads"
    print $ maybeadd_crazy3 (Nothing) (Nothing)
    print $ maybeadd_crazy3 (Nothing) (Nothing)
    print $ maybeadd_crazy3 (Just 2) (Just 1)

    print $ "The DO-MONADS ... "
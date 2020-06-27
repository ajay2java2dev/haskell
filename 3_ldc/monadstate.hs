-- state monad introduction: https://www.youtube.com/watch?v=Y8X0HqCn5DI



-- Defining Functor type class
-- State type had to be in certain order
    -- State type has to come first and then the result type
    -- Functors need a Container type and hence State comes first
        -- State s a : means s is State with value of a within it.
instance Functor (State s) where
    fmap :: (a -> b) -> (State s a) -> (State s b)
    fmap f g = State (\s1 -> let (x, s2) = runState g s1 in (f x, s2))


instance Applicative (State s) where
    pure x = State (\s -> (x,s))
    f1 <*> x1 = State (\s -> let    (f,s2) = runState f1 s
                                    (x,s3) = runState x1 s2
                             in     (f x, s3))

instance Monad (State s) w
    return = pure --return type
    --bind
    x >>= f = State (\s -> let (y,s2)   = runState x s
                               (z, s3)  = runState (f y) s2
                            in (z,s3)))



main :: IO()
main = do
    
    let x10 = runState ex2a 10
    print $ x10

    let x11 = runState (fmap inc ex2a) 10
    print x11
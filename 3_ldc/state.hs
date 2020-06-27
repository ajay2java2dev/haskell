{-# LANGUAGE InstanceSigs #-}

--Defining types

ex1::Integer -> (Integer, Integer) -- Incoming Integer is the state, Output tuple is a result and a state
ex1 s = (s * 2, s + 1)

--encapsulation
newtype State s a = State { runState :: s -> (a,s)} -- record syntax format, s = output type and a = state type
ex2a :: State Integer Integer
ex2a = State {runState = ex1} -- runstate declared specifically

ex2b :: State Integer Integer
ex2b = State ex1 -- no runState. matched automatically

inc x = x + 1

-- To run a monad, a functor and applicative need to be defined.
-- Functor takes a container / wrapper type. Here (State s a) is the container with value a in it
instance Functor (State s) where
    fmap :: (a -> b) -> (State s a) -> (State s b)
    fmap f g = State (\s1 -> let (x, s2) = runState g s1 in (f x, s2))

instance Applicative (State s) where
    pure x = State (\s -> (x,s))
    f1 <*> x1 = State (\s ->    let     (f,s2)  = runState f1 s 
                                        (x, s3) = runState x1 s2 in (f x, s3))

instance Monad (State s) where
    return  = pure
    x >>= f = State (\s ->  let (y,s2)  =   runState x s
                                (z,s3)  =   runState (f y) s2 in (z,s3))


incState (State f) = State (\s -> let (x,s0) = f s in (x, s0 + 1))
--or
incState2 f = State (\s -> let (x,s0) = runState f s in (x, s0 + 1))

-- state manipulation
get :: State s s -- whatever portion the get function gets its going to copied over to the value portion of the tuple 
get = State (\s -> (s,s))

put :: a -> State a () -- put is used to replace the incoming variable x
put x = State (\s -> ((),x))

{--
*Main> :info Monad
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


--type State = Int -- Label for Int is State
--type M a = State -> (a, State) -- Label for this function is M

{--
M acts as a type constructor and a represents a concrete type
M Takes a concrete type and out comes another concrete type
*Main> :k M
 M :: * -> *
*Main> :k State0
State :: *
--}
{--
instance Monad M where
    return a = \s -> (a, s)
    -- here m holds the monadic value of m (a) and k holds the function (a -> m b). The o/p is monadic type of m of b
    (>>=) m k = \s ->   let (a,y) = (m s) in 
                        let (b,z) = k a y in (b, z)
--}

main :: IO()
main = do
    print $ ex1 10
    print $ runState ex2a 100
    print $ runState ex2b 12
    
    --incState (State f) = State (\s -> let (x,s0) = f s in (x, s0 +1))


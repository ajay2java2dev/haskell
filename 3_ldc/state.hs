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


type State = Int -- Label for Int is State
type M a = State -> (a, State) -- Label for this function is M

{--
M acts as a type constructor and a represents a concrete type
M Takes a concrete type and out comes another concrete type
*Main> :k M
 M :: * -> *
*Main> :k State0
State :: *
--}

instance Monad M where
    return a = \s -> (a, s)
    -- here m holds the monadic value of m (a) and k holds the function (a -> m b). The o/p is monadic type of m of b
    (>>=) m k = \s ->   let (a,y) = (m s) in 
                        let (b,z) = k a y in (b, z)

main :: IO()
main = do
    print ""
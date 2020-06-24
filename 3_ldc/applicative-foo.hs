{--
LAWS OF APPLICATIVES:
    - Identity      --> pure id <*> v               = v
    - Composition   --> pure (.) <*> u <*> v <*> w  = u <*> (v <*> w)
    - Homomorphishm --> pure f <*> pure x           = pure (f x)
    - Interchange   --> u <*> pure y                = pure ($ y) <*> u
--}

-- Haskell Wikibooks page : https://en.wikibooks.org/wiki/Haskell/Applicative_functors

import Control.Applicative

data Foo a = Foo a

instance Show a => Show (Foo a) where
    show (Foo a) = "Foo " ++ show a

instance Functor Foo where
    fmap func (Foo a) = Foo $ func a

instance Applicative Foo where
    pure = Foo
    (Foo f) <*> (Foo z) = Foo $ f z
    -- or written as
    --(Foo f) <*> (Foo z) = Foo (f z)

inc = (+1)

x1_old = fmap inc (Foo 30)
x1_new = inc <$> (Foo 30) -- just a synonum for fmap. Both above and this are same
-- NOTE : Rem regular $ sign is the Function Application.

x2 = Foo (+1) <*> (Foo 20)
-- or can also be written as
x2_new  = Foo inc <*> (Foo 20)

plus a b = a + b




main :: IO ()
main = do
    print x1_old    -- ans : 31
    print x1_new    -- ans : 31

    print x2        -- ans : 21
    print x2_new    -- ans : 21

    print $ plus <$> (Foo 20) <*> (Foo 30)  -- ans: 50
    print $ (+) <$> (Foo 20) <*> (Foo 30)   -- ans: 50
    print $ (*) <$> (Foo 20) <*> (Foo 30)   -- ans: 600
-- simple example of parametrized types and applying types

x :: Int -> Int
x = (+1)

data Foo = Foo Int

instance Eq Foo where
    (==) (Foo i) (Foo j) = i == j

x1 = Foo 19
y1 = Foo 19

main :: IO()
main = do
    print $ x1 == y1
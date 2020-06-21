class Frober a where
    frob :: a -> (String, Integer)

data A = A {aValue :: Int}
instance Frober A where
    frob a = let value = aValue a in (show value, toInteger value)


data B = B {bValue :: Integer}
instance Frober B where
    frob b = let value = bValue b in (show value, toInteger value)


data C = C {cValue :: Double}
instance Frober C where
    frob c = let value = cValue c in (show value, round value)

printFrobResult :: Frober a => a -> IO ()
printFrobResult = print . frob

main :: IO ()
main = do
    printFrobResult (A 100)
    printFrobResult (B (2 ^ 70))
    printFrobResult (C 3.141)

{--

    O/P:

    ("100",100)
    ("1180591620717411303424",1180591620717411303424)
    ("3.141",3)
    
--}
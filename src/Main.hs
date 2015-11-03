import NumericalMethods.Simple

main :: IO ()
main = do
     let v = foldr (\x y -> insertElement x y) E [1..500]
     let a = show v
     putStrLn a

{-# LANGUAGE Haskell2010, CPP #-}

module NumericalMethods.Simple where

import Data.List

-- | Acceleration due to gravity.
gravityAccel :: Double
gravityAccel = 9.8

printAngle :: Maybe Double -> String
printAngle x = case x of
             Just r -> show r
             Nothing -> "Bad angles"

epsilon :: Double
epsilon = 0.002


type Position = (Double, Double)
type Force = (Double, Double)
type Velocity = (Double, Double)
type Vertex = (Double, Double)
type Mass = Double

data Vector a = Vector a a a deriving (Show, Eq)

instance Functor Vector where
  fmap f (Vector a b c) = Vector (f a) (f b) (f c)

data Color = Red | Black deriving Show
data RedblackTree a = E
  | T Color (RedblackTree a) a (RedblackTree a) deriving Show

member :: Ord a => a -> RedblackTree a -> Bool
member _ E = False
member x (T _ a y b)
   | x < y = member x a
   | x > y = member x b
   | otherwise = True

-- insertElement :: Integer -> RedblackTree -> RedblackTree
-- insertElement x E = RedblackTree Red E x E
-- insertElement x (RedblackTree c l y r)
--  | x < y = RedblackTree c (insertElement x l) y r
--  | x > y = RedblackTree c l y (insertElement x r)
--  | otherwise = RedblackTree c l y r

balance :: Ord a => Color -> RedblackTree a -> a -> RedblackTree a -> RedblackTree a 
balance Black (T Red (T Red a x b) y c) z d = T Red (T Black a x b) y (T Black c z d)
balance Black (T Red a x (T Red b y c)) z d = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red (T Red b y c) z d) = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red b y (T Red c z d)) = T Red (T Black a x b) y (T Black c z d)
balance color a x b = T color a x b
    
insertElement :: Ord a => a -> RedblackTree a -> RedblackTree a
insertElement x s = T Black a y b
                where ins E = T Red E x E
                      ins s@(T color a y b)  
                          | x < y = balance color (ins a) y b
                          | x > y = balance color a y (ins b)
                          | otherwise = s
                      T _ a y b = ins s


e1 :: RedblackTree Integer
e1 = T Black (T Red E 5 E) 6 (T Red E 40 E)


--instance Applicative Vector where
--  pure = Vector

--instance Num Vector where
--  (+) (Vector a b c) (Vector i j k) = Vector (a + i) (b + j) (c + k)

exampleVector1 :: Vector Double
exampleVector1 = Vector 1.1 2.2 3.3

exampleVector2 :: Vector Double
exampleVector2 = Vector 5.6 7.8 9.0

-- | Triangle data structure.
data TriangleInfo =
  Triangle Vertex Vertex Vertex
  deriving (Show)

subVector :: Vertex -> Vertex -> Vertex
subVector v0 v1 = (fst v0 - fst v1, snd v0 - snd v1)


-- binary tree
data Tree a = Node a (Tree a) (Tree a)
              | Empty
              deriving (Show, Eq)

-- | example tree
someTree :: Tree Int
someTree = Node 5 (Node 3 Empty Empty) (Node 2 Empty Empty)

-- | search a binary tree for an element.
searchTree :: Eq a => a -> Tree a -> Bool
searchTree e (Node c l r)
    | c == e = True
    | otherwise = searchTree e l || searchTree e r


searchTree _ Empty = False

-- hypotenuse :: Floating a => a -> a -> a
-- hypotenuse l w = sqrt squaredH
--   where squaredH = l^2 +w^2


-- areaTriangle :: Triangle -> Double
-- areaTriangle a = 0.5 *

data Ball
    =
      Ball
      !Mass -- mass
      !Force -- force
      !Position
      !Velocity
       deriving (Show, Eq)


-- | Newton-Raphson method for finding roots.
newtonRaphson :: Double -> (Double -> Double) -> Double
newtonRaphson guess f
  | difference <= epsilon = newguess
  | otherwise = newtonRaphson newguess f
  where
    newguess = guess - (f guess)/(fprime guess)
    difference = abs(newguess - guess)
    fprime = derivative f

-- | Newton's method for finding optimization of functions.
optimize :: RealFunction -> Double -> Double
optimize f guess = newtonRaphson guess g
  where g = (\x -> derivative2 f x / deriv_second f x)

-- | Numerical method for finding the square root.
mysqrt :: Double -> Double -> Double
mysqrt a x
  | difference <= epsilon = newguess
  | otherwise = mysqrt a newguess
  where
    newguess = (1 / 2) * (x + a/x)
    difference = abs(newguess - x)

-- | returns an approximation of the derivative
-- using forward differences.
derivative :: RealFunction -> RealFunction
derivative f x = (f (x + epsilon) - f x)/epsilon

-- | returns an approximation of the derivative using the symmetric
-- difference quotient.
derivative2 :: RealFunction -> RealFunction
derivative2 f x = (f (x + epsilon) - f (x - epsilon)) / (2*epsilon)

-- finite central differences.
derivative3 :: RealFunction -> RealFunction
derivative3 f x = (d - 8*c + 8*b - a)/(12*epsilon)
      where
      a = f (x + 2*epsilon)
      b = f (x + epsilon)
      c = f (x - epsilon)
      d = f (x - 2*epsilon)

-- | returns the second derivative of a function.
deriv_second :: RealFunction -> RealFunction
deriv_second f x = (f (x + epsilon) - 2*f(x) + f(x - epsilon) ) / epsilon**2

deriv :: (Double->Double) -> Double -> Double
deriv f x = (f (x + dx) - f x) / dx
    where dx = epsilon*x



-- Some example functions that I don't use
-- g :: (RealFloat a) => a -> a
-- g x = x^3 - 4*x + 1

-- g2 :: (RealFloat a) => a -> a -> a
-- g2 y x = x*y

type RealFunction = (Double->Double)

map2D :: RealFunction -> RealFunction -> (Double,Double) -> (Double, Double)
map2D fx fy (x, y) = (fx x, fy y)

-- update
-- updateBall :: Ball -> Double -> Ball
-- updateBall (Ball m f p v) _ = Ball m f p v
--  where
--    acceleration = force / m
--    force = 5

-- view

-- model
initBall :: Ball
initBall = Ball 5 (0,0) (0, 0) (1, 0)

-- eulerIter
-- differential equation, starting y, timestep
eulerIter :: RealFunction -> Double -> Double -> [Double]
eulerIter f h y0 =
  let it y = euler f h y
  in
    iterate it y0

-- Euler method
euler :: RealFunction -> Double -> Double -> Double
euler f h y0 = y0 + h * f y0

-- | The 'eulerBackwards' function takes the implicit Euler of a function.
eulerBackwards ::
    RealFunction ->
    Double ->
    Double ->
    Double

eulerBackwards f h y0 = newtonRaphson guess g
  where
    guess = euler f h y0
    g = (\x -> x - y0 - h*(f x))

infinitelist :: [Double]
infinitelist = 1.0 : map (+ 0.01) infinitelist
-- function, x0, y0

showValues :: Show a => [a] -> String
showValues x = intercalate "\n" $ map show x



--eulerIter :: (Double->Double) -> Double -> [Double]
--eulerIter f y0 = y0 : eulerIter f yp
--  where
--    yp = eulerBackwards f y0

{-eulerIter :: (Double->Double) -> Double -> Double -> [(Double,Double)]
eulerIter f x0 y0 = x
  where
    dx = 0.001
    yp = euler f y0
    x = 1.0 : map (+ dx) x
    -}

-- Modified Euler method
{-
eulerModified :: (RealFloat a) => (a->a->a) -> a -> a -> a
eulerModified f y0 x0 = y0 + h/2 * ( yp0 + yp1)
  where
    yp0 = f y0 x0
    yp1 = euler f y0 x0
    h = 0.0001
-}

mag :: [Double] -> Double
mag [] = 0
mag v = sqrt(sumsqr)
  where sumsqr = foldr (\x y -> x**2 + y) 0 v

dot :: [Double] -> [Double] -> Maybe Double
dot [] [] = Just 0.0
dot _ [] = Nothing
dot [] _ = Nothing
dot (x:xs) (y:ys) =  case dot xs ys of
   Just r -> Just(x * y + r)
   Nothing -> Nothing

myreverse :: [Char] -> [Char]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]


angle :: [Double] -> [Double] -> Maybe Double
angle a b = case dot a b of
  Just r -> Just( acos (r / (mag a * mag b)))
  Nothing -> Nothing

say :: String -> IO ()
say x = putStrLn $ "Buy " ++ x

say2 :: String -> IO Bool
say2 x = do
  putStrLn x
  return True

groceries :: [String]
groceries = ["ham","tomato","garlic"]

buyItem :: [String] -> String -> [String]
buyItem list item = [item] ++ list

-- gprime :: (RealFloat a) => a -> a
-- gprime x = 3*x^2 - 4

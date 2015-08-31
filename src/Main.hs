-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main,
    mysqrt,
    newtonRaphson,
    deriv
) where

-- This strang looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
-- >>> import Data.List (stripPrefix)

-- | Simple function to create a hello message.
-- prop> stripPrefix "Hello " (hello s) == Just s
hello :: String -> String
hello s = "Hello " ++ s

main :: IO ()
main = putStrLn (hello "World")
-- main = putStrLn (mysqrt 2 1)

newtonRaphson :: (RealFloat a) => a -> (a -> a) -> a
newtonRaphson guess f
    | difference <= epsilon = newguess
    | otherwise = newtonRaphson newguess f
    where
        newguess = guess - f guess/fprime guess
        difference = abs(newguess - guess)
        epsilon = 0.002
        fprime = deriv f

mysqrt :: (RealFloat a) => a -> a -> a
mysqrt a x
    | difference <= epsilon = newguess
    | otherwise = mysqrt a newguess
    where
        newguess = 1 / 2 * (x + a/x)
        difference = abs(newguess - x)
        epsilon = 0.02

deriv :: (RealFloat a) => (a->a) -> a -> a
deriv f x = (f (x + dx) - f x) / dx
    where dx = 0.0001*x

g :: (RealFloat a) => a -> a
g x = x^3 - 4*x + 1

g2 :: (RealFloat a) => a -> a -> a
g2 y x = x*y

-- Euler method
euler :: (RealFloat a) => (a->a->a) -> a -> a -> a
euler f yzero xzero = yzero + h * f yzero xzero
    where h = 0.1

-- Modified Euler method
eulerModified :: (RealFloat a) => (a->a->a) -> a -> a -> a
eulerModified f y0 x0 = y0 + h/2 * ( yp0 + yp1)
    where
    yp0 = f y0 x0
    yp1 = euler f y0 x0
    h = 0.0001


-- gprime :: (RealFloat a) => a -> a
-- gprime x = 3*x^2 - 4


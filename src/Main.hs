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
    newtonRaphson
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

newtonRaphson :: (RealFloat a) => a -> (a -> a) -> (a -> a) -> a
newtonRaphson guess f fprime
    | difference <= epsilon = newguess
    | otherwise = newtonRaphson newguess f fprime
    where
        newguess = guess - f guess/fprime guess
        difference = abs(newguess - guess)
        epsilon = 0.002

mysqrt :: (RealFloat a) => a -> a -> a
mysqrt a x
    | difference <= epsilon = newguess
    | otherwise = mysqrt a newguess
    where
        newguess = 1 / 2 * (x + a/x)
        difference = abs(newguess - x)
        epsilon = 0.02

g :: (RealFloat a) => a -> a
g x = x^3 - 4*x + 1

gprime :: (RealFloat a) => a -> a
gprime x = 3*x^2 - 4


module Samples where
import System.IO

{- Functional Friday Haskell Playground!

This file serves as an initial playground to get comfortable with the
syntax and some features of haskell.
-}

createSalutation :: String -> String -> String
createSalutation salutation entity = salutation ++ ", " ++ entity

sayHello :: String -> String
sayHello = createSalutation "Hello"

printNumber :: Int -> String
printNumber x = show x

fibs :: Integral a => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

takeFibs :: Integral a => Int -> [a]
takeFibs = \amnt -> take amnt fibs

reverseWords :: String -> String
reverseWords = unwords . reverse .words

biggestPrimeFactor :: Integer -> Integer
biggestPrimeFactor = head . filter isPrime . factors
  where
  intSqrt = ceiling . sqrt . fromIntegral
  isPrime n = let h = intSqrt n in
          [] == [x | x <- [2..h], n `mod` x == 0]
  factors n =
      let h  = intSqrt n
          lf = [x | x <- [2..h], n `mod` x == 0]
      in map (div n) lf ++ reverse lf

primes :: [Integer]
primes = primes' [2..]
       where
       primes' [] = []
       primes' (p:[]) = [p]
       primes' (p:xs) = p : primes' [x | x<-xs, x `mod` p > 0]


respondWith :: String -> IO ()
respondWith message = do
  currentMessage <- hGetLine stdin
  if currentMessage == "quit"
    then
        putStrLn "Fine, I Quit!"
    else
        (putStrLn $ message ++ currentMessage) >> respondWith message


main :: IO ()
main = putStrLn "say a command" >> respondWith "Sorry, I Don't Understand: "

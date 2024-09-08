-- Fibbing
-- Create a program that will calculate the Fibonacci Sequence to 10 places.
-- Extensions:
--   1. Allow the user to specify the number of places generated
--   2. Print this in reverse order
--   3. Display the total of all the numbers shown

import Data.Foldable (forM_)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

fib :: (Eq t, Num t, Num a) => t -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
        putStr "Places: "
        hFlush stdout
        places <- readLn :: IO Int
        let nums = fib <$> reverse [0 .. places] :: [Int]
         in do
                forM_ nums print
                printf "Total: %i\n" $ sum nums

-- Palindromes
-- Write a program that checks if a string entered by the user is a palindrome. A palindrome is a word that reads the same forwards as backwards like "racecar"

module Main where

main :: IO ()
main = interact $ unlines . fmap (show . isPalindrome) . lines

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

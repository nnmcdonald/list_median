-- median.hs
-- Nathaniel McDonald
-- 22 Mar 2018
--
-- CSCE A331 Spring 2018
-- Solutions to Assignment 5 Exercise C
--
-- Prompts user to enter Integers, one per line, 
-- then prints the median value
--
-- ** The program does not error check the input, so **
-- ** it will fail for anything but an Integer       **

module Main where

import System.IO
import Data.List

main = do
   putStrLn "Enter list of integers with one on each line, enter a blank line to end"
   putStrLn "I will output the median."
   hFlush stdout
   n <- getList
   check_list n
   putStrLn "Enter another list? y/n"
   again <- getChar
   putStrLn ""
   read_input again
   return ()

--read_input
read_input y = if y == 'y' then main else do return ()

--check_list
--Output if the list entered was empty
check_list [] = do
   putStrLn "Empty list entered."
   return ()

--For non-empty list, prints the median value
check_list xs = do
   m <- median xs
   putStrLn "The median value is : "
   print m
   return ()

--getList
--Reads input from user and makes a list
getList = do
   putStrLn "Enter number (blank line to end): "
   line <- getLine
   if line == "" 
     then return []
   else fmap ((read line :: Integer):) getList

--listLength
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

--median
--Returns the median value as IO
median :: [Integer] -> IO (Integer)
median xs = return (findMedian (sort xs) ((div (listLength xs) 2) + 1))

--findMedian
--Extracts median from the list
findMedian :: [Integer] -> Integer -> Integer
findMedian (x:_) 1 = x
findMedian (_:xs) n = findMedian xs (n-1)





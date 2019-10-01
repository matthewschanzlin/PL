{- |
Module      :  Assignment1
Description :  Assignment 1 submission for <CS 4400 / CS 5400 (choose one)>.
Copyright   :  (c) Matthew Schanzlin

Maintainer  :  schanzlin.ma@husky.neu.edu
-}

module Assignment1 where

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

isIntegerElem :: Integer -> [Integer] -> Bool
isIntegerElem x [] = False
isIntegerElem x (a:as) = x == a || isIntegerElem x as

isTypeElem :: Eq a => a -> [a] -> Bool
isTypeElem x [] = False
isTypeElem x (a:as) = x == a || isTypeElem x as

count :: Eq a => a -> [a] -> Integer
count x [] = 0
count x (a:as)
  | x == a = 1 + count x as
  | otherwise = count x as

data Tree a = Node (Tree a) a (Tree a)
            | Empty
            deriving (Show, Eq)

tree1 :: Tree Integer
tree1 = Node (Node (Node (Node Empty 30 Empty) 40 (Node Empty 50 Empty))
                   15
                   (Node Empty 50 Empty))
             18
             (Node (Node Empty 8 (Node Empty 13 Empty))
                   20
                   Empty)

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

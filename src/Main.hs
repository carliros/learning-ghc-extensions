{-# LANGUAGE ViewPatterns #-}
module Main where

{- ######### View Patterns extension -}
-- example taken from https://jsdw.me/posts/haskell-language-extensions/#viewpatterns
-- The general idea is that you can execute the case pattern match in any computation. 

someList :: [(String, Int)]
someList = [("n1", 1), ("n2", 2)]

getOrDefault :: String -> Int
getOrDefault name = case (lookup name someList) of
  Just v -> v
  Nothing -> 5

getOrDefault' :: String -> Int
getOrDefault' (\n -> lookup n someList -> Just v) = v
getOrDefault' _ = 5

-- I wonder how it would be with a bigger data type, will it execute the computation each time?
data Contact = Person String | Institute String | Reference String

main :: IO ()
main = do
  putStrLn "hello world"

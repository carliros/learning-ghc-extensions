{-# LANGUAGE LambdaCase #-}

-- Docs: https://riptutorial.com/haskell/example/5689/lambdacase
-- LambdaCase extension helps you write a pattern matching function
-- without repeating the parameter, it is a lambda case function.

module GhcExtension.LambdaCase where

-- examples taken from docs
dayOfTheWeek' :: Int -> String
dayOfTheWeek' 0 = "Sunday"
dayOfTheWeek' 1 = "Monday"
dayOfTheWeek' 2 = "Tuesday"
dayOfTheWeek' 3 = "Wednesday"
dayOfTheWeek' 4 = "Thursday"
dayOfTheWeek' 5 = "Friday"
dayOfTheWeek' 6 = "Saturday"

dayOfTheWeek'' :: Int -> String
dayOfTheWeek'' i = case i of
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"

dayOfTheWeek :: Int -> String
dayOfTheWeek = \case
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"

equals :: Bool
equals = f1 == f2 && f2 == f3 && f3 == "Monday"
    where f1 = dayOfTheWeek' 1
          f2 = dayOfTheWeek'' 1
          f3 = dayOfTheWeek 1

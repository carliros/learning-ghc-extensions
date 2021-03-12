{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- Docs: https://jsdw.me/posts/haskell-language-extensions/#patternsynonyms
-- Define a pattern synonym is like a "type" but for value patterns. 
-- It matches some patther and gives to it a name. This match can be in both 
-- directions (left to right and right to left) or just on one direction.

module GhcExtension.PatternSynonyms where
import Text.Printf (printf)

data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
    deriving (Eq, Show)

data Time = Time { hour :: Int, minute :: Int }
    deriving (Eq, Show)

data DayTime = DayTime { day :: Day, time :: Time }
    deriving (Eq, Show)

-- print time with padding
printTime :: Time -> String
printTime (Time h m) = pad (show h) ++ ":" ++ pad (show m)
    where pad l@(_:_:_) = l
          pad l@(_:_)   = "0"++l

printDayTime :: DayTime -> String
printDayTime (DayTime d t) = printf "It is %s, %s" (show d) (printTime t)

-- lets define some pattern synonyms... 
-- it gives the impression that we are defining a new "type", but we are not.
pattern SundayNoon :: DayTime
pattern SundayNoon = DayTime Sunday (Time 12 00)

pattern MidnightFriday :: DayTime
pattern MidnightFriday = DayTime Friday (Time 00 00)

-- we can define pattern with variables
pattern Sun ::  Int -> Int -> DayTime
pattern Sun h m = DayTime Sunday (Time h m)

pattern Mon :: Int -> Int -> DayTime
pattern Mon h m = DayTime Monday (Time h m)

-- until now we defined pattern on both directions, which means a matching of one to one on values.
-- now, let's define some patterns that matched many values. This is a one direction pattern.

pattern AnytimeSun :: DayTime
pattern AnytimeSun <- DayTime Sunday _

pattern AnytimeMon :: DayTime
pattern AnytimeMon <- DayTime Monday _

-- Important: Notice that one direction patterns (<-) can only be used on function declaration,
-- not where raw values are used. For example, we can not use AnytimeMon in calling a 
-- function like "whichDay AnytimeMon", that will throw an error. However, we indeed can
-- use "whichDay SundayNoon" because SundayNoon is both direction pattern (=)

-- we can write a function to use this pattern
whichDay :: DayTime -> String
whichDay AnytimeMon = "Monday!"
whichDay AnytimeSun = "Sunday!"
whichDay _ = "Some other day!"

-- we can rewrite whichDay to retrieve the time using get function provided by records
whichDay' :: DayTime -> String
whichDay' d@AnytimeSun = printf "Sunday %s" ( printTime $ time d)
whichDay' d@AnytimeMon = printf "Monday %s" (printTime $ time d)
whichDay' _ = "Other."

-- it is also possible to define one way pattern using variables
pattern MonT :: Time -> DayTime
pattern MonT t <- DayTime Monday t

pattern SunT :: Time -> DayTime
pattern SunT t <- DayTime Sunday t

-- and rewrite out whichDay function
whichDay'' :: DayTime -> String
whichDay'' (SunT t) = printf "Sunday %s" ( printTime t)
whichDay'' (MonT t) = printf "Monday %s" (printTime t)
whichDay'' _ = "Other day."

-- One cool thing about Pattern Synonyms and View Pattern is that we can combine them to create cool patterns
isMorningHour :: Int -> Bool
isMorningHour t = t < 12

pattern Morning :: DayTime
pattern Morning <- DayTime _ (Time (isMorningHour -> True) _)

timeOfDay :: DayTime -> String
timeOfDay Morning = "It is morning"
timeOfDay _ = "It is not morning"

-- it is possible to return a different type than values in the view pattern
isMorningHour' :: Int -> (Bool, Int)
isMorningHour' t = (t < 12, t)

pattern MorningT :: Int -> Int -> DayTime
pattern MorningT h m <- DayTime _ (Time (isMorningHour' -> (True, h)) m)

timeOfDay' :: DayTime -> String
timeOfDay' (MorningT h m) = printf "It is morning, %s:%s" (show h) (show m)
timeOfDay' _ = printf "It is not morning"

-- Important: we have to practice the use of view patterns, it does not require 
-- the extra parameter to  sent. Review the definition of MorningT.

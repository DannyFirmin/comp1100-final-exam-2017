module Date where

type Date = (Int,Int,Int)

{-|
  The function 'showDate' converts a 'Date' triple into a string for that date.
  Make sure that days ending in 1 have a suffix "st", days ending in 2 have a suffix of "nd",
  and days ending in 3 have a suffix of "rd".  Otherwise the day has a suffix of "th".

  Examples:

  >>> showDate (1,1,2001)
  "1st January, 2001"

  >>> showDate (2,2,2002)
  "2nd February, 2002"

  >>> showDate (3,3,2003)
  "3rd March, 2003"

  >>> showDate (4,4,2004)
  "4th April, 2004"
-}
showDate :: Date -> String
showDate (d,m,y) = undefined -- TODO

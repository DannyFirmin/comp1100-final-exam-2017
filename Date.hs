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
-- showDate :: Date -> String
-- showDate (d,m,y)
--   |m==1 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "January, " ++ show y
--   |m==1 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "January, " ++ show y
--   |m==1 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "January, " ++ show y
--   |m==1 = show d ++ "th " ++ "January, " ++ show y
--
--   |m==2 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "February, " ++ show y
--   |m==2 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "February, " ++ show y
--   |m==2 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "February, " ++ show y
--   |m==2 = show d ++ "th " ++ "February, " ++ show y
--
--   |m==3 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "March, " ++ show y
--   |m==3 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "March, " ++ show y
--   |m==3 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "March, " ++ show y
--   |m==3 = show d ++ "th " ++ "March, " ++ show y
--
--   |m==4 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "April, " ++ show y
--   |m==4 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "April, " ++ show y
--   |m==4 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "April, " ++ show y
--   |m==4 = show d ++ "th " ++ "April, " ++ show y
--
--   |m==5 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "May, " ++ show y
--   |m==5 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "May, " ++ show y
--   |m==5 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "May, " ++ show y
--   |m==5 = show d ++ "th " ++ "May, " ++ show y
--
--   |m==6 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "June, " ++ show y
--   |m==6 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "June, " ++ show y
--   |m==6 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "June, " ++ show y
--   |m==6 = show d ++ "th " ++ "June, " ++ show y
--
--   |m==7 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "July, " ++ show y
--   |m==7 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "July, " ++ show y
--   |m==7 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "July, " ++ show y
--   |m==7 = show d ++ "th " ++ "July, " ++ show y
--
--   |m==8 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "August, " ++ show y
--   |m==8 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "August, " ++ show y
--   |m==8 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "August, " ++ show y
--   |m==8 = show d ++ "th " ++ "August, " ++ show y
--
--   |m==9 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "September, " ++ show y
--   |m==9 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "September, " ++ show y
--   |m==9 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "September, " ++ show y
--   |m==9 = show d ++ "th " ++ "September, " ++ show y
--
--   |m==10 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "October, " ++ show y
--   |m==10 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "October, " ++ show y
--   |m==10 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "October, " ++ show y
--   |m==10 = show d ++ "th " ++ "October, " ++ show y
--
--   |m==11 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "November, " ++ show y
--   |m==11 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "November, " ++ show y
--   |m==11 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "November, " ++ show y
--   |m==11 = show d ++ "th " ++ "November, " ++ show y
--
--   |m==12 && (d==1 || d==11 || d==21 || d==31) = show d ++"st " ++ "December, " ++ show y
--   |m==12 && (d==2 || d==12 || d==22) = show d ++"nd " ++ "December, " ++ show y
--   |m==12 && (d==3 || d==13 || d==23) = show d ++"rd " ++ "December, " ++ show y
--   |m==12 = show d ++ "th " ++ "December, " ++ show y
--
--   |otherwise = error"no such day"
showDate :: Date -> String
showDate (d,m,y)
  |d==1 || d==21||d==31 = show d ++ "st "++ showMonth m ++ ", " ++ show y
  |d==2 ||d==22 = show d ++ "nd "++ showMonth m ++", " ++ show y
  |d==3 ||d==23 = show d ++ "rd "++ showMonth m ++", " ++ show y
  |otherwise = show d ++ "th "++ showMonth m ++", " ++ show y
    where
      showMonth :: Int -> String
      showMonth m
       | m==1 ="January"
       | m==2 ="February"
       | m==3 ="March"
       | m==4 ="April"
       | m==5 ="May"
       | m==6 ="June"
       | m==7 ="July"
       | m==8 ="August"
       | m==9 ="September"
       |m==10 ="October"
       |m==11 ="November"
       |m==12 ="December"
       |otherwise = error"Error"
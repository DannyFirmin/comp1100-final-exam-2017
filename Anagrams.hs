{-|
  To compile this program, type the following command (omitting the >) into the terminal:

  > ghc -o anagrams AnagramsMain.hs

  This invokes the Glasgow Haskell Compiler (ghc)
  and creates an executable file anagrams.

  You can then run the program with a command like:

  > ./anagrams 6 < dictionary

  This reads the file dictionary and prints the 6-letter
  anagrams to the terminal. If you want the result to go
  to an output file called "results", you can type:

  > ./anagrams 6 < dictionary > results

  Alternatively, you can load this file into GHCi to test functions individually:

  > ghci Anagrams.hs
-}
module Anagrams where

-- The functions toLower and sort are provided in standard Haskell libraries:
import Data.Char (toLower)
import Data.List (sort)

-- We need getArgs because the ./anagrams command has
-- an argument, namely the number of letters in a word.
import System.Environment (getArgs)

-- We hide the Prelude type Word so we can define our own:
import Prelude hiding (Word)

type Word = [Char]

type Key = [Char]

{-|
  The 'anagrams' function takes a list of English words in alphabetical order,
  extracts just the 'n'-letter words, and produces a string that, when
  displayed, gives a list of the anagram entries for the 'n'-letter words.

  Examples:

  >>> putStr (anagrams 2 ["ab","ba","xy","yx","xyzzy"])
  ab: ab,ba
  xy: xy,yx

  >>> putStr (anagrams 5 ["ab","ba","xyzzy"])
  xyyzz: xyzzy
-}
anagrams :: Int -> [Word] -> String
anagrams n = showResult . groupByKey . sort . map addKey . getWords n

{-|
  The 'getWords' function extracts the words of length n from a list of words.

  Examples:

  >>> getWords 5 ["Brave","New","World"]
  ["Brave","World"]
-}
getWords :: Int -> [Word] -> [Word]
getWords n = filter (\x -> length x==n)

--   |n>0 && length [c]==n && ([c]:[cs]) /= [] = [c]: getWords n [cs]
--   |n>0 && length [c]/=n && ([c]:[cs]) /= [] =  getWords n [cs]
--   |otherwise = []

{-|
  The 'addKey' function takes a word and pairs it with its key.
  The key consists of the characters of the word, sorted into alphabetical order.

  Examples:

  >>> addKey "word"
  ("dorw","word")
-}
addKey :: Word -> (Key, Word)
addKey n = (sort n, n)

-- | An 'Entry' associates an anagram key with the list of words corresponding to that key.
type Entry = (Key, [Word])

{-|
  The 'groupByKey' function replaces each group of adjacent key-word pairs that have the same key with a
  single dictionary 'Entry' in which the first component is the common key and the second component is the list of
  words associated with that key.

  Examples:

  >>> groupByKey [("ab","ab"),("ab","ba")]
  [("ab",["ab","ba"])]

  >>> groupByKey [("ab","ab"),("xy","xy"),("ab","ba")]
  [("ab",["ab"]),("xy",["xy"]),("ab",["ba"])]
-}

groupByKey :: [(Key, Word)] -> [Entry]
-- groupByKey (x:xs) = case x:xs of
--  [x] -> [(fst x, [snd x])]
--  [x, y] ->  if fst x == fst y then [(fst x,fst x : [snd y])] else [(fst x, [fst x]), (fst y, [snd y])]
--  x:y:xs
--      |fst x == fst (head xs)  -> (fst x,fst x : [snd (head xs)]): tail (groupByKey (y:xs))
--      |otherwise -> (fst x,[fst x]): tail (groupByKey (y:xs))

groupByKey []=[]
groupByKey [(k1,w1)] = [(k1,[w1])]
groupByKey [(k1,w1),(k2,w2)]
  |k1==k2 = [(k1,[w1,w2])]
  |otherwise = [(k1,[w1]),(k2,[w2])]
groupByKey (x:y:xs) = groupByKey [x,y] ++ groupByKey xs
-- !!!
-- groupByKey [] = []
-- groupByKey [(a,b)] =  [(a,[b])]
-- groupByKey [(a,b),(d,c)]
--   | a==d = [(a,[b,c])]
--   | otherwise = [(a,[b]),(d,[c])]
-- groupByKey [(a,b),(c,d),(e,f)]
--   |a==c && c==e = [(a,[b,d,f])]
--   |a==c && c/=e = groupByKey [(a,b),(c,d)]
--   |a/=c && c==e = head(groupByKey [(c,d),(e,f)]):[]
--   |otherwise = [(a,[b]),(d,[c]),(e,[f])]
-- groupByKey (x:y:k:xs) =  groupByKey [x,y,k] ++  (groupByKey (k:xs))
-- !!!

-- groupByKey (x:y:xs) = removeT ( groupByKey [x,y] ++  (groupByKey (y:xs)))
--
-- removeT :: [Entry] -> [Entry]
-- removeT [] = []
-- removeT [x] = [x]
-- removeT (x:y:xs)
--   |x==y = removeT (y:xs)
--   |otherwise = x : removeT (y:xs)
{-|
  The 'showResult' function produces a printable string from a list of association entries.

  Example:

  >>> putStr $ showResult [("ab",["ab","ba"]),("xy",["xy","yx"])]
  ab: ab,ba
  xy: xy,yx
-}
showResult :: [Entry] -> String
-- showResult (x:xs) = case x:xs of
--  [] -> []
--  [x]-> fst x ++ ": " ++ a ++  "," ++ b ++ "\n"
--   where
--      [a, b] = snd x
--  x:xs -> fst x ++ ": " ++ a ++ "," ++ b ++ "\n" ++ showResult xs
--   where
--      [a, b] = snd x -- I think I'm wrong. Because the method I used only fit the given doctest
--      -- what if there is more or less element than 2 in the list of second tuple?
--      -- e.g. run [("ab",["ab","ba"]),("ab",["ab"]),("xy",["xy","yx"])]
--      -- get an error: Irrefutable pattern failed for pattern [a, b]
--      -- because ("ab",["ab"]) only has one element in the list ["ab"] so it cannot match [a,b]
--      -- My question is, how can I fit all the conditions?
showResult list = case list of
  [] -> ""
  ((a,[]):xs) -> a ++ ": " ++ showlistOfList [] ++ "\n" ++ showResult xs
  ((a,x:xs):ys) -> a ++ ": " ++ showlistOfList (x:xs) ++ "\n" ++ showResult ys

showlistOfList :: [Word] -> String
showlistOfList [] = ""
showlistOfList [b,c] = b ++ "," ++ c
showlistOfList (x:xs) = x ++ showlistOfList xs

main :: IO ()
main = do
  [n] <- getArgs
  text <- getContents
  putStr (anagrams (read n) (words text))
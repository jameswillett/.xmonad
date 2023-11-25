module Utils
  ( clampLength
  , trim
  , wrap
  , wrapAtt
  , wrapColor
  ) where

import Data.Char (isSpace)
import Data.List (intercalate, dropWhile, dropWhileEnd)

clampLength :: Int -> String -> String
clampLength n string = clamped ++ (if string /= clamped then ellipses else "")
  where
    ellipses = "..."
    breakpoint = n - 4
    clamped = take breakpoint string

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

wrap pre suff string = pre ++ string ++ suff

wrapAtt :: String -> String
wrapAtt = wrap "{{" "}}"

wrapColor hex = wrap ("<fc=#" ++ hex ++ ">") "</fc>"

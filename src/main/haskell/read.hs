module Read (SExpr(AtomSym, AtomNum, List), sread) where

import TestHelpers
import Test.HUnit

data SExpr = AtomSym String | AtomNum Integer | List [SExpr]
  deriving (Show, Eq)

sread :: String -> SExpr
sread s = let (sexpr, _) = readWithRest s in sexpr

readL :: String -> SExpr -> (SExpr,String)
readL (')' : tail) (List acc) = (List acc, tail)
readL (x : xs) (List acc) = let (next, rest) = readWithRest(x : xs) in readL rest (List (acc ++ [next]))
readL _ _ = error "unterminated list"

ends = [' ', ')', ']']
readChars :: String -> String -> (SExpr, String)
readChars (c : tail) acc
  | elem c ends = (symOrNum acc, c : tail)  
  | otherwise   = readChars tail (acc ++ [c]) 
readChars [] acc = (symOrNum acc, [])

symOrNum :: String -> SExpr
symOrNum s = if isInteger s then AtomNum (read s :: Integer) else AtomSym s

readWithRest :: String -> (SExpr,String)
readWithRest (' ' : tail) = readWithRest tail
readWithRest ('(' : tail) = readL tail (List [])
--readWithRest ('"' : tail) = readStringLit tail ['"']
readWithRest (c : tail) = readChars (c : tail) []

--readStringLit :: String -> String -> (SExpr, String)
--readStringLit ('"' : tail) acc = (StringLit (acc ++ ['"']), tail)
--readStringLit (c : tail) acc = readStringLit tail (acc ++ [c])

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

--------------------
------ tests -------
--------------------

results = runTests
  [
    makeTest "7" (sread "7") (AtomNum 7),
    makeTest "x" (sread "x") (AtomSym "x"),
    makeTest "(7)" (sread ("(7)")) (List [AtomNum 7])
  ]

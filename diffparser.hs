module Main where

import Data.List
import System.Environment

type Line = String (Int, Int, Int, Int)

data Diffline = Addition Line
              | Deletion Line
              | Context Line
      deriving (Show)

type Diff = [Diffline]

scanner :: String -> [Diffline]
scanner s = if " +" `isPrefixOf` s
	then [Addition (drop 2 s)]
	else if " -" `isPrefixOf` s
		then [Deletion (drop 2 s)]
		else [Context s]

data Dim = Num
  deriving (Show)

data V a = Ctx a
         | CC Dim a a
      deriving (Show)

type ChoiceRep = [V Line]

parser :: [Diffline] -> [V Line]
parser [] = []
parser ((Context t):xs) = [Ctx t] ++ (parser xs)
parser ((Addition t):xs) = [CC A [] t] ++ (parser xs)
parser ((Deletion t):xs) = [CC B t []] ++ (parser xs)

data MergePattern = Moves
                  | None

 movepattern :: ChoiceRep -> MergePattern
 movepattern cr = if ((CC A [] t) `elem` cr) && ((CC B t []) `elem` cr)
 	then Moves
 	else None


main = do
	[f]  <- getArgs
	text <- readFile f
	let linesF = lines text
	print $ map scanner linesF




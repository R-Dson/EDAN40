
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

string1 = "writers"
string2 = "vintner"

type AlignmentType = (String, String)

optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments a b c s t = [("","")]

similarityScore :: String -> String -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (s:ss) (t:ts)
  | (s == '-') || (t == '-') = scoreSpace + m
  | s == t = scoreMatch + similarityScore ss ts
  | s /= t = scoreMismatch + similarityScore ss ts
  | otherwise = m
    where m = max (similarityScore ss (t:ts)) (similarityScore (s:ss) ts)

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs =
  let vfs = [(x, valueFcn x) | x <- xs]
      m = maximum [snd v | v <- vfs]
      res = filter (\i -> snd i == m) vfs
  in [fst r | r <- res]


main :: IO ()
--main = print (similarityScore "H A S K E L L" "P A S C A - L")
main = print (similarityScore "HASKELL" "PASCA-L")
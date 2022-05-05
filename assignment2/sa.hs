
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

string1 = "writers"
string2 = "vintner"


optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments a b c s t = [("","")]

-- a)
similarityScore :: String -> String -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (s:ss) (t:ts)
  | (s == '-') || (t == '-') = scoreSpace + m
  | s == t = scoreMatch + similarityScore ss ts
  | s /= t = scoreMismatch + similarityScore ss ts
  | otherwise = m
    where m = max (similarityScore ss (t:ts)) (similarityScore (s:ss) ts)

-- b)
-- appends h1 to the first element in the pair of each element in aList
-- and appends h2 to the second element in the pair of each element in aList
-- as example: attachHeads 'x' 'y' [("a", "b")] would return [("xa", "yb")]
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- c)
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs =
  let vfs = [(x, valueFcn x) | x <- xs]
      m = maximum [snd v | v <- vfs]
      res = filter (\i -> snd i == m) vfs
  in [fst r | r <- res]


-- d)
type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] s = [([], s)]
optAlignments s [] = [(s, [])]

optAlignments (s:ss) (t:ts) = maximaBy (uncurry similarityScore) l
  where
    v = attachHeads s t $ optAlignments ss ts
    w = attachHeads '-' t $ optAlignments (s:ss) ts
    u = attachHeads s '-' $ optAlignments ss (t:ts)
    l = concat [v, w, u]



main :: IO ()
--main = print (similarityScore "H A S K E L L" "P A S C A - L")
main = print (similarityScore "HASKELL" "PASCA-L")
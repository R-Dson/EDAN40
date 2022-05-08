
-- Writen by:
-- Robin Baki Davidsson (ro5226ba-s)
-- Verneri Sirva (ve7517si-s)
-- (Group 33 on canvas)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

string3 = "writ"
string4 = "vint"

optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optimalAlignments a b c s t = [("","")]

-- given hints
--score(x,'-') = score('-',y) = scoreSpace
--score(x,y) = scoreMatch, if x == y
--             scoreMismatch, if x /= y


--sim((x:xs),(y:ys)) = max {sim(xs,ys) + score(x,y),
--                          sim(xs,(y:ys)) + score(x,'-'),
--                          sim((x:xs),ys) + score('-',y)}

-- in code we get score and a)
score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y    = scoreMatch
    | otherwise = scoreMismatch

-- a)
similarityScore :: String -> String -> Int
similarityScore ss [] = (length ss) * scoreSpace 
similarityScore [] ss = (length ss) * scoreSpace
similarityScore (s:ss) (t:ts)
  | (s == '-') || (t == '-') = scoreSpace + m
  | s == t = scoreMatch + m
  | otherwise = scoreMismatch + similarityScore ss ts
    where m = maximum [similarityScore ss ts, similarityScore ss (t:ts), similarityScore (s:ss) ts]

-- Getting more than 3 results from "outputOptAlignments string1 string2" with this one..
-- I think I misunderstood it when i wrote this
--similarityScore [] ss = (length ss) * scoreSpace;
--similarityScore ss [] = (length ss) * scoreSpace;
--similarityScore (s:ss) (t:ts) = maximum [similarityScore ss ts + score s t,
--                                         similarityScore (s:ss) ts + score '-' t,
--                                         similarityScore ss (t:ts) + score s '-']

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
optAlignments [] [] = [([],[])]
optAlignments [] (s:ss) = attachHeads '-' s $ optAlignments [] ss
optAlignments (s:ss) [] = attachHeads s '-' $ optAlignments ss []
optAlignments (s:ss) (t:ts) = maximaBy (uncurry similarityScore) $ concat [v, w, u]
  where
    v = attachHeads s t $ optAlignments ss ts
    u = attachHeads s '-' $ optAlignments ss (t:ts)
    w = attachHeads '-' t $ optAlignments (s:ss) ts

-- e)
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 =
  let ans = optAlignments string1 string2
      m = ("There are " ++ show(length ans) ++ " optimal alignments") : map formater ans
  in mapM_ putStrLn m

formater :: (String, String) -> String
formater (s, t) = s ++ " : " ++ t

main :: IO ()
--main = print (similarityScore "H A S K E L L" "P A S C A - L")
main = print (similarityScore "HASKELL" "PASCA-L")
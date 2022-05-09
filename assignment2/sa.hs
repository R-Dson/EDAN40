import Text.Read (Lexeme(String))
import Text.ParserCombinators.ReadP (string)

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


similarityScore' :: String -> String -> Int
similarityScore' xs ys = simLen (length xs) (length ys)
  where
    simLen i j  = simTable  !!i!!j
    simTable = [[ simEntry' i j | j<-[0..]] | i<-[0..] ]

    simEntry' :: Int -> Int -> Int
    simEntry' _ 0 = scoreSpace
    simEntry' 0 _ = scoreSpace
    simEntry' i j
      | (x == '-') || (y == '-') = scoreSpace + m
      | x == y = scoreMatch + m
      | otherwise = scoreMismatch + simLen i j
        where m = maximum [simLen i j,
                          simLen i (j-1),
                          simLen (i-1) j]
              x = xs!!(i-1)
              y = ys!!(j-1)

    {-simEntry' :: Int -> Int -> String -> String -> Int
    simEntry' _ 0 ss [] = (length ss) * scoreSpace
    simEntry' 0 _ [] ss = (length ss) * scoreSpace
    simEntry' i j (s:ss) (t:ts)
      | (s == '-') || (t == '-') = scoreSpace + m
      | s == t = scoreMatch + m
      | otherwise = scoreMismatch + simLen i j xs ys
        where m = maximum [simLen i j xs ys, 
                          simLen i (j-1) ss (t:ts), 
                          simLen (i-1) j (s:ss) ts]

    simEntry :: Int -> Int -> Int
    simEntry _ 0 = 0
    simEntry 0 _ = 0
    simEntry i j
      | x == y    = 1 + simLen (i-1) (j-1)
      | otherwise = max (simLen i (j-1))
                        (simLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)-}

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

maximaBy' :: Ord b => (a -> b) -> [a] -> [(a,b)]
maximaBy' valueFcn xs =
  let vfs = [(x, valueFcn x) | x <- xs]
      m = maximum [snd v | v <- vfs]
      res = filter (\i -> snd i == m) vfs
  in res

-- d)
type AlignmentType = (String, String)

--optAlignments :: String -> String -> [AlignmentType]
-- new
--optAlignments :: String -> String -> ([Int], [AlignmentType])
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments [] (s:ss) = attachHeads '-' s $ optAlignments [] ss
optAlignments (s:ss) [] = attachHeads s '-' $ optAlignments ss []
--optAlignments (s:ss) (t:ts) = (l, maximaBy (uncurry similarityScore) list)
optAlignments (s:ss) (t:ts) = ans
  where
    v = attachHeads s t $ optAlignments ss ts
    u = attachHeads s '-' $ optAlignments ss (t:ts)
    w = attachHeads '-' t $ optAlignments (s:ss) ts
    list = concat [v, w, u];
    temp = maximaBy' (uncurry similarityScore) list
    ans = map fst temp
    m = maximum $ map snd temp

optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = map (\(a, b) -> (reverse a, reverse b))
                          (snd $ alignLen (length xs) (length ys))
  where
    alignLen :: Int -> Int -> (Int, [AlignmentType])
    alignLen i j = alignTable!!i!!j

    alignTable :: [[(Int, [AlignmentType])]]
    alignTable = [[ alignEntry i j | j <- [0..] ] | i <- [0..] ]

    alignEntry :: Int -> Int -> (Int, [AlignmentType])
    alignEntry 0 0 = (0, [("", "")])
    alignEntry i 0 = (i * scoreSpace, [(take i xs, take i (repeat '-'))])
    alignEntry 0 j = (j * scoreSpace, [(take j (repeat '-'), take j ys)])
    alignEntry i j = (m, ans)
      where
        x = xs!!(i-1)
        y = ys!!(j-1)
        v = toTuples (alignLen (i-1) (j-1)) x y
        u = toTuples (alignLen (i-1) j) x '-'
        w = toTuples (alignLen i (j-1)) '-' y
        list = [v, u, w]
        temp = maximaBy' (uncurry similarityScore) $ concatMap snd list
        ans = map fst temp
        m = maximum $ map snd temp

        toTuples val a b = (fst val, attachHeads a b $ snd val)


-- e)
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 =
  let ans = optAlignments' string1 string2
      m = ("There are " ++ show(length ( ans)) ++ ", optimal: " ++ show ( ans) ++ ". Optimal alignments") : map formater (ans)
  in mapM_ putStrLn m

formater :: (String, String) -> String
formater (s, t) = s ++ " : " ++ t

main :: IO ()
--main = print (similarityScore "H A S K E L L" "P A S C A - L")
main = print (similarityScore "HASKELL" "PASCA-L")
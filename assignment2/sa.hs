
-- Writen by:
-- Robin Baki Davidsson (ro5226ba-s)
-- Verneri Sirva (ve7517si-s)
-- (Group 33 on canvas)

{-
  We did utilize the hint section that was given to us on the 
  assignment page after trying to write code without it but got 
  a bit lost. Some of the issues we experienced was in the 
  beginning when we tried to structure similarityScore with the 
  score function but we found a solution that gives the proper 
  answers. We left the similarityScore that we had issues with 
  commented out, to show our failure and what worked instead. 

  In section 3 when we were to rewrite the similarityScore and 
  optAlignments functions we used the naming similarityScore' 
  and optAlignments' to differentiate between them. 

  We found it hard to pick just one part that we thought was 
  the most difficult to write but implementing the tables and
  such from the mcsLength example given did take a lot of time
  spent just thinking of how it is structured and how we should
  change it for our use case. 

  Since we were kind of given the structure in mcsLength for 
  the dynamic programming problem with tables and such, we felt
  that we are more proud of the code writen for similarityScore
  and optAlignments. Also because we just extended these functions
  tho make use of the code structure of mcsLength. 
-}

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

string3 = "writ"
string4 = "vint"

string5 = "aferociousmonadatemyhamster"
string6 = "functionalprogrammingrules"

string7 = "bananrepubliksinvasionsarmestabsadjutant"
string8 = "kontrabasfiolfodralmakarmästarlärling"

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
    where m = maximum [similarityScore ss ts, 
                       similarityScore ss (t:ts), 
                       similarityScore (s:ss) ts]


similarityScore' :: String -> String -> Int
similarityScore' xs ys = simLen (length xs) (length ys)
  where
    simLen :: Int -> Int -> Int
    simLen i j  = simTable  !!i!!j
    simTable :: [[Int]]
    simTable = [[simEntry' i j | j<-[0..]] | i<-[0..]]

    simEntry' :: Int -> Int -> Int
    simEntry' 0 0 = 0
    simEntry' i 0 = scoreSpace * i
    simEntry' 0 j = scoreSpace * j
    
    -- Used hint section. 
    simEntry' i j = maximum [simLen (i-1) (j-1) + score x y,
                             simLen i (j-1) + score x '-',
                             simLen (i-1) j + score '-' y]
        {- Old code to show what didn't work
        | (x == '-') || (y == '-') = m
        | x == y = m 
        | otherwise = simLen (i-1) (j-1) + score x y
        -}
          where
            x = xs!!(i-1)
            y = ys!!(j-1)
            {-
            m = maximum [simLen (i-1) (j-1) + score x y,
                          simLen i (j-1) + score x '-',
                          simLen (i-1) j + score '-' y]
            -}

-- Left to show some of what we tried.
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
-- as example: attachHeads 'x' 'y' [("a", "b")] would return [("xa", "yb")].
-- Another way to write this function is 
-- attachHeads h1 h2 = map (\(x,y) -> (h1:x,h2:y))
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- c)
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs =
  let vfs = [(x, valueFcn x) | x <- xs]
      m = maximum [snd v | v <- vfs]
      res = filter (\i -> snd i == m) vfs
  in map fst res --[fst r | r <- res]

-- d)
type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments [] (s:ss) = attachHeads '-' s $ optAlignments [] ss
optAlignments (s:ss) [] = attachHeads s '-' $ optAlignments ss []
optAlignments (s:ss) (t:ts) = ans
  where -- getting the optimal alignments of the combinations
    v = attachHeads s t $ optAlignments ss ts
    u = attachHeads s '-' $ optAlignments ss (t:ts)
    w = attachHeads '-' t $ optAlignments (s:ss) ts
    list = concat [v, w, u];
    ans = maximaBy (uncurry similarityScore) list

-- using the given mcsLength to structure a new optAlignments as optAlignments'
optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = 
  let align = alignScore (length xs) (length ys)
  in [(reverse x, reverse y) | (x,y) <- snd align]
  where
    alignScore :: Int -> Int -> (Int, [AlignmentType])
    alignScore i j = alignTable!!i!!j

    alignTable :: [[(Int, [AlignmentType])]]
    alignTable = [[alignEntry i j | j <- [0..] ] | i <- [0..]]

    alignEntry :: Int -> Int -> (Int, [AlignmentType])
    alignEntry 0 0 = (0, [("", "")]) -- not sure if this one is needed
    alignEntry i 0 = (i * scoreSpace, [(take i xs, take i (repeat '-'))])
    alignEntry 0 j = (j * scoreSpace, [(take j (repeat '-'), take j ys)])
    alignEntry i j = (m, ans)
      where
        -- getting the letters
        x = xs!!(i-1)
        y = ys!!(j-1)
        
        -- calculating the alignment value, creating the 3 cases
        v = toTuples (alignScore (i-1) (j-1)) x y $ score x y
        u = toTuples (alignScore (i-1) j) x '-' $ score x '-'
        w = toTuples (alignScore i (j-1)) '-' y $ score x '-'
        list = [v, u, w]

        -- this line find the maximum of the first values in the list, 
        -- the first value is the score
        t = maximaBy fst list

        -- second entry in t is the strings as [AlignmentType]
        ans = concatMap snd t

        -- first value is the maximum alignment value
        m = maximum $ map fst list

        -- help function to convert the inputs to (score, [AlignmentType])
        toTuples val a b i = (fst val + i, attachHeads a b $ snd val)

-- e)
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 =
  let ans = optAlignments' string1 string2
      m = ("There are " ++ show(length  ans) ++ " optimal alignments: ") : map formater ans
  in mapM_ putStrLn m

formater :: (String, String) -> String
formater (s, t) = s ++ " : " ++ t

main :: IO ()
main = outputOptAlignments string7 string8
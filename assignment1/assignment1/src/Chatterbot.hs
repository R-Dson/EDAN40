module Chatterbot where
import Utilities
import System.Random
import Data.Char


chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain =
  do
    r <- randomIO :: IO Float;
    let list = [ (fst x, pick r (snd x)) | x <- brain ]
    return $ rulesApply list 

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply phrasepair phrase =
  case word of
    Nothing -> words ""
    Just a -> a
  where word = (\i -> transformationsApply "*" id phrasepair i) (reflect phrase)

--"please help me" -> phrasepair -> första delen -> match första delen med INPUT -> 
--"please *" -> "help me" -> phrasepair -> andra delen -> "*" -> "help me"
reflect :: Phrase -> Phrase
reflect phrase = [ let l = lookup x reflections
                    in case l of
                      Nothing -> x
                      Just a -> concat $ substitute x [x] [a]
                      | x <- phrase ]

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
-- botbrain is structured as [(Phrase, [Phrase])]
-- foreach element in input we create a pair where we use "map2" to apply "words . map toLower" to the first 
-- element of the input pair to making it a Phrase and lowercase. The second element of the pair becomes 
-- "map words (snd x)" which is a list of answers the robot can reply.
rulesCompile xs = [(words (map toLower (fst x)), map words (snd x)) | x <- xs]

--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

-- Using the function 'transformationsApply' we wrote earlier. Applies the function using fix for the values in xs and p.
reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply xs p = fix (\i -> case (transformationsApply "*" id xs i) of
                                  Nothing -> i
                                  Just a -> a) p


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute a (x:xs) ys
  | a == x = ys ++ substitute a xs ys
  | otherwise = x : substitute a xs ys

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wildcard p s
  -- if they're equal, continue searching
  | pi == si = match wildcard (tail p) (tail s)
  -- if we reach the wildcard. We only look for the first value recursively, 
  -- thus orElse throws away the second value
  | pi == wildcard = orElse (singleWildcardMatch p s) $ longerWildcardMatch p s
  | otherwise = Nothing
  where
    pi = head p
    si = head s

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
-- we know that x is the answer as the first correct element since 
-- match call this when pi == wildcard. This will always return 
-- Just [x], or nothing of course if there is no matches
--singleWildcardMatch (wc:ps) (x:xs) =
  --mmap (const [x]) $ match wc ps xs
-- this was given in lecture 6 slide 28
singleWildcardMatch (wc:ps) (x:xs) = match wc ps xs >> Just [x]


-- appends the correct x (using (x:)) and then continues looking for new
-- values AFTER x that are not equal to the elements in wc:ps
-- for example, "a=*;" "a=32;", first we get x = 3, then we continue
-- looking but in the match loop pi == si will not be equal but pi == wildcard
-- will be equal so that means now 2 will also be appended, thus we have 32. 
-- next iteration will reach the ; and result in pi == si being true finaly reaching Nothing
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) $ match wc (wc:ps) xs


-- findInd '*' "a=*;" $ reverse $ findInd '*' "a=*;" "a=32;"
-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ [] _ = Nothing
transformationApply _ _ _ ([],_) = Nothing
transformationApply _ _ _ (_,[]) = Nothing
transformationApply b f xs (ys, zs) = mmap (substitute b zs) (match b ys xs)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply b f (p:pairs) xs = orElse (transformationApply b f xs p) (transformationsApply b f pairs xs)
module Parser(module CoreParser, require, (-#), (#-), spaces, (>->), accept, (#), word, lit, number, iter) where
import Prelude hiding (return, fail)
import CoreParser
import Data.Char
infixl 7 -#, #-

type T a = Parser a

letter :: CoreParser.Parser Char
letter = char ? isAlpha

chars :: Int -> Parser String
chars 0 = return []
chars i = char # chars (i-1) >-> cons

require :: String -> Parser String
require w = accept w ! err ("error " ++ w) 

spaces :: Parser String
spaces = iter space

space :: Parser Char
space = char ? isSpace

(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

bldNumber :: Int -> Int -> Int
bldNumber n d = 10*n+d
-- had some issues with the type when using bldNumber but this seems to work
number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

accept :: String -> Parser String
accept w = token (chars (length w) ? (==w))

letters :: Parser String
letters = iter letter

token :: Parser a -> Parser a
token m = m #- space

word :: Parser String
word = token letters

lit :: Char -> Parser Char
lit c = token char ? (==c)
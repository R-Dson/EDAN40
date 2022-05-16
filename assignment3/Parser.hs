module Parser(module CoreParser, require, (-#), (#-), spaces, (>->), accept, (#)) where
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

token :: Parser a -> Parser a
token m = m #- space

accept :: String -> Parser String
accept w = token (chars (length w) ? (==w))

-- word
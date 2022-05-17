-- CoreParser.hs
-- defines the Parser type and implements the three elementary parsers, char, return and fail, and the basic parser operators #, !, ?, #>, and >->, described in Lennart Andersson's Parsing with Haskell.

-- The class Parse with signatures for parse, toString, and fromString with an implementation for the last one is introduced.

-- The representation of the Parser type is visible outside the module, but this visibilty should not be exploited.

module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),
                  Parse, parse, toString, fromString) where

import Prelude hiding (return, fail)

infixl 3 ! 
infixl 7 ?
infixl 6 #
infixl 5 >->
infixl 4 #>

class Parse a where
    parse :: Parser a
    fromString :: String -> a
    fromString cs =
        case parse cs of
               Just(s, []) -> s
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"
    toString :: a -> String

type Parser a = String -> Maybe (a, String)

-- Define three elementary parsers
char :: Parser Char

char []= Nothing

char (c:cs) = Just (c, cs)

return :: a -> Parser a
return a cs = Just (a, cs)

fail ::  Parser a 
fail cs = Nothing

-- Define the required parser operators
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
    Nothing -> n cs 
    mcs -> mcs


(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs = 
    case m cs of
        Nothing -> Nothing
        Just(r, s) -> if p r then Just(r, s) else Nothing

(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs = 
    case m cs of
    Nothing -> Nothing
    Just(a, cs') -> 
        case n cs' of
            Nothing -> Nothing
            Just(b, cs'') -> Just((a, b), cs'')

(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs = 
    case m cs of
        Just (a, cs') -> Just (b a, cs')
        Nothing -> Nothing

(#>) :: Parser a -> (a -> Parser b) -> Parser b 
(p #> k) cs = 
    case p cs of
        Nothing -> Nothing
        Just (a, cs') -> k a cs'
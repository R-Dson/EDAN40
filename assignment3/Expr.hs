module Expr(Expr, T, parse, fromString, value, toString) where
import Prelude hiding (return, fail)
import Parser
import qualified Dictionary

type T = Expr

data Expr =
    Num Int | Var String | Add Expr Expr |
    Sub Expr Expr | Mul Expr Expr | Div Expr Expr

value (Var e) dictionary = 
    case Dictionary.lookup e dictionary of
        Just v -> v
        Nothing -> error ("Not found")

value (Num e) dictionary = e
value (Add x y) dictionary = (value x dictionary) + (value y dictionary)
value (Sub x y) dictionary = (value x dictionary) - (value y dictionary)
value (Mul x y) dictionary = (value x dictionary) * (value y dictionary)
value (Div x y) dictionary = case value y dictionary of
    0 -> error ""
    _ -> div (value x dictionary) (value y dictionary)

-- confused, what to do...
--instance Parse Expr where
--    parse = expr
module Expr(Expr, T, parse, {-fromString,-} value, {-toString,-}) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

type T = Expr

data Expr =
    Num Int | Var String | Add Expr Expr |
    Sub Expr Expr | Mul Expr Expr | Div Expr Expr

var :: Parser Expr
var = word >-> Var
num :: Parser Expr
num = number >-> Num -- not sure what's the problem

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

mulOp = lit '*'>-> (\_ -> Mul) ! lit '/' >-> (\_ -> Div)
addOp = lit '+'>-> (\_ -> Add) ! lit '-' >-> (\_ -> Sub)

factor :: Parser Expr
factor = num ! var ! lit '(' -# var #- lit ')'

bldOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
bldOp e (oper,e') = oper e e'

term' :: Expr -> Parser Expr
term' e = mulOp # factor >-> bldOp e #> term' !return e

term :: Parser Expr
term = factor #> term'

expr' :: Expr -> Parser Expr
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr :: Parser Expr
expr = term #> expr'

-- confused, what to do...
instance Parse Expr where
    parse = expr
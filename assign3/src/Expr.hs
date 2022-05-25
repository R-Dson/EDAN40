module Expr(Expr, T, parse, fromString, value, toString) where
-- Writen by:
-- Robin Baki Davidsson (ro5226ba-s)
-- Verneri Sirva (ve7517si-s)
-- (Group 33 on canvas)

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

type T = Expr

data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Exp Expr Expr
         deriving Show

var, num, factor, term, expr, expf :: Parser Expr

var = word >-> Var
num = number >-> Num

value :: Expr -> Dictionary.T String Integer -> Integer
value (Var e) dictionary =
    case Dictionary.lookup e dictionary of
        Just v -> v
        Nothing -> error ("undefined variable " ++ e)
value (Num e) dictionary = e
value (Add x y) dictionary = (value x dictionary) + (value y dictionary)
value (Sub x y) dictionary = (value x dictionary) - (value y dictionary)
value (Mul x y) dictionary = (value x dictionary) * (value y dictionary)
value (Div x y) dictionary = case value y dictionary of
    0 -> error "division by 0"
    _ -> div (value x dictionary) (value y dictionary)
value (Exp x y) dictionary = (value x dictionary) ^ (value y dictionary)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = lit '*'>-> const Mul ! lit '/' >-> const Div

addOp :: Parser (Expr -> Expr -> Expr)
addOp = lit '+'>-> const Add ! lit '-' >-> const Sub

expOp :: Parser (Expr -> Expr -> Expr)
expOp = lit '^' >-> const Exp

bldOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
bldOp e (oper, e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

exp', term', expr' :: Expr -> Parser Expr
exp' e = expOp # expf >-> bldOp e #> exp' ! return e
expf = factor #> exp' -- changed priority

term' e = mulOp # expf >-> bldOp e #> term' ! return e
term = expf #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Exp t u) = parens (prec>7) (shw 7 t ++ "^" ++ shw 8 u)

instance Parse Expr where
    parse = expr
    toString = shw 0

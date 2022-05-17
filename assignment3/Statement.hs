module Statement (parse) where

import Parser hiding (T)
import Expr
import Dictionary

type T = Statement

data Statement = Skip
    | Begin [Statement]
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Write Expr.T
    | Read String -- variable name
    | Assignment String Expr.T -- variable assignment
    | Comment String

-- need word 
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss -- 
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin -- finds "begin", parse until "end" to get final Statement
skip = accept "skip" # require ";" >-> buildSkip -- finds "skip" and the ";"
ifCase = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf -- NOT SURE WHY ITS BROKEN
whileCase = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
commentCase = accept "--" -# word #- require "\n" >-> buildComment
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite


buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e
buildBegin :: [Statement] -> Statement
buildBegin statement = Begin statement
buildSkip :: p -> Statement
buildSkip _ = Skip
buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (expr, state) = While expr state
buildWrite :: Expr.T -> Statement
buildWrite expr = Write expr
buildRead :: String -> Statement
buildRead str = Read str
buildIf :: ((a -> b -> c, a), b) -> c
buildIf ((expr, statement), oStatement) = expr statement oStatement
buildComment :: String -> Statement
buildComment str = Comment str

instance Parse Statement where
    parse = assignment ! begin ! ifCase ! commentCase ! whileCase ! skip 


exec :: [Statement.T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment s e:es) dict i = []
exec (Skip : sk) dict i = []
exec (Begin s:statements) dict i = []
exec (If e s1 s2:sta) dict i = []
exec (While e s:statements) dict i = []
exec (Read s:ss) dict i = []
exec (Write e:es) dict i = []

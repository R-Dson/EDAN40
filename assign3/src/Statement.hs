-- Writen by:
-- Robin Baki Davidsson (ro5226ba-s)
-- Verneri Sirva (ve7517si-s)
-- (Group 33 on canvas)

module Statement (Statement.T, parse, exec, toString) where

import Parser hiding (T)
import Expr
import Dictionary

type T = Statement

data Statement = Skip
    | Assignment String Expr.T -- variable assignment
    | Begin [Statement]
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Write Expr.T -- assign
    | Read String -- variable name
    | Comment String
    deriving Show

-- need word 
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss -- 
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin -- finds "begin", parse until "end" to get final Statement
skip = accept "skip" # require ";" >-> buildSkip -- finds "skip" and the ";"
ifCase = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
whileCase = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
commentCase = accept "--" -# cmnt #- require "\n" >-> buildComment
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
reader = accept "read" -# word #- require ";" >-> buildRead

buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e

buildBegin :: [Statement] -> Statement
buildBegin = Begin

buildSkip :: p -> Statement
buildSkip _ = Skip

buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (expr, state) = While expr state

buildWrite :: Expr.T -> Statement
buildWrite = Write

buildRead :: String -> Statement
buildRead r = Read r

buildIf :: ((Expr , Statement), Statement) -> Statement
buildIf ((expr, statement), oStatement) = If expr statement oStatement

buildComment :: String -> Statement
buildComment = Comment

-- spacing to read easier
exec :: [Statement.T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (Assignment s e:es) dict i = 
    exec es (Dictionary.insert (s, Expr.value e dict) dict) i

exec (Skip : sk) dict i = 
    exec sk dict i

exec (Begin s:stmts) dict i = 
    exec (s ++ stmts) dict i

exec (If e thenStmts elseStmts:stmts) dict i =
    if Expr.value e dict > 0
    then exec (thenStmts : stmts) dict i
    else exec (elseStmts : stmts) dict i

exec (While e s:stmts) dict i =
    if Expr.value e dict > 0
    then exec (s : (While e s) : stmts) dict i
    else exec stmts dict i

exec (Read s:ss) dict i = 
    exec ss (Dictionary.insert (s, head i) dict) (tail i)

exec (Write e:es) dict i = 
    Expr.value e dict : exec es dict i

exec (Comment c:cs) dict i = 
    exec cs dict i -- handled as white space

shw :: Statement.T -> String
shw (Assignment v e) = v ++ " := " ++ (Expr.toString e) ++ ";\n"
shw Skip = "skip;\n"
shw (Read v) = "read " ++ v ++ ";\n"
shw (Write e) = "write " ++ Expr.toString e ++ "\n"
shw (If e s es) = "if " ++ Expr.toString e ++ "\n then " ++ shw s ++ " else " ++ shw es
shw (While e s) = "while " ++ Expr.toString e ++ " do \n " ++ shw s ++ "\n"
shw (Begin s) = "begin\n " ++ concatMap shw s ++ " end"
shw (Comment c) = "-- " ++ c ++ "\n"

instance Parse Statement where
    parse = assignment ! begin ! ifCase ! commentCase ! whileCase ! skip ! reader ! write
    toString = shw
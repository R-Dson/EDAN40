module Statement ( parse, toString, fromString) where

import Parser(Parser, require, (#-), (>->), accept, (#)) 
import Expr
type T = Statement

data Statement = Skip
    | Begin [Statement]
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Write Expr.T
    | Read String -- variable name
    | Assignment String Expr.T -- variable assignment

-- need word 
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss (v, e) = Assignment v e
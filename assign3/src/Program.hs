module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Parse T where
  parse = iter Statement.parse >-> Program 
  -- Maybe we should have stmt or stmts instead of p? p seems fine 👍
  toString (Program p) = concatMap Statement.toString p -- using statement's toString

exec :: T -> [Integer] -> [Integer]
exec (Program p) = \i -> Statement.exec p Dictionary.empty i -- Using statement's exec with empty dictionary


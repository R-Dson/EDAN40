module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Parse T where
  parse = error "Program.parse not implemented"
  -- Maybe we should have stmt or stmts instead of p? 
  toString (Program p) = error "Program.toString not implemented"

exec (Program p) = error "Program.exec not implemented"

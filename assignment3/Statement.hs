import Expr (Expr, T)

type T = Statement

data Statement = Skip
    | Begin [Statement]
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Write Expr.T
    | Read String -- variable name
    | Assignment String Expr.T -- variable assignment
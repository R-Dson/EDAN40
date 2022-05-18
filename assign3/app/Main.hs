module Main where

import Program
import Parser

src = "read k; read n; m:=1;"
p = Parser.fromString src

main :: IO ()
main = print(Program.exec p [3,16])
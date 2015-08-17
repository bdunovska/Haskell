module AbstractSyntax where

type Identifier = String

{-

The operations are listed in the order of precedence as in the
languages C and C++ (from low to high), where all the operations in
the same line have the same precedence:

-}

data OpName = Or                                 -- |
            | And                                -- &
            | Eq                                 -- ==
            | Leq | Less | Geq | Greater         -- <=  <  >=  >
            | Add | Sub                          -- + -
            | Mul | Div | Mod                    -- * / %
            | Not                                -- !
            deriving (Eq, Show)

data Expr = Constant Int
          | Var Identifier
          | Op OpName [Expr]
          deriving (Eq, Show)

data Program = Identifier := Expr
             | IfThenElse Expr Program Program
             | IfThen Expr Program
             | While Expr Program
             | Block [Program]
             deriving (Eq, Show)

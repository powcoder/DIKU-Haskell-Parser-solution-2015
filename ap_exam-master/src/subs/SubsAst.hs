https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module SubsAst where

data Program = Prog [Stm]
             deriving (Show, Eq)

data Stm = VarDecl Ident (Maybe Expr)
         | ExprAsStm Expr
         deriving (Show, Eq)

data Expr = Number Int
          | String String
          | Array [Expr]
          | Undefined
          | TrueConst
          | FalseConst
          | Var Ident
          | Compr ArrayFor Expr
          | Call FunName [Expr]
          | Assign Ident Expr
          | Comma Expr Expr
          deriving (Show, Eq)

type ArrayFor = (Ident, Expr, Maybe ArrayCompr)

data ArrayCompr = ArrayForCompr ArrayFor
                | ArrayIf Expr (Maybe ArrayCompr)
                deriving (Eq, Show)

type Ident = String
type FunName = String

{-# LANGUAGE DeriveFunctor #-}
module CataAna where

import           Data.Foldable                  ( )
import           Control.Arrow                  ( (>>>)
                                                , (<<<)
                                                , (|||)
                                                , (&&&)
                                                )
data Expr a
      = Literal { intVal :: Int }
      | Ident   { name :: String  }
      | Index   { target :: a, idx :: a }
      | Unary   { op :: String, target :: a }
      | Binary  { lhs :: a, op :: String, rhs :: a }
      | Call    { func :: a, args :: [a] }
      | Paren   { target :: a }
      deriving (Show, Eq, Functor)

newtype Term f = In {out :: f (Term f)}

type Algebra f a = f a -> a

ten, add, call :: Term Expr
ten = In Literal {intVal = 10}
add = In Ident {name = "add"}
call = In Call {func = add, args = [ten, ten]}

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn

mystery :: Functor f => Algebra f a -> Term f -> a
mystery fn = out >>> fmap (mystery fn) >>> fn

countNodes :: Algebra Expr Int
countNodes (Unary _ arg) = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args) = fn + sum args + 1
countNodes (Index it idx) = it + idx + 1
countNodes (Paren arg) = arg + 1
countNodes (Literal _) = 1
countNodes (Ident _) = 1


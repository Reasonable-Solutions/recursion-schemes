{-# Language DeriveFunctor #-}
module Lib
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Expr a
      = Literal { intVal :: Int}
      | Ident   { name :: String}
      | Index   { target :: a, idx :: a }
      | Unary   { op :: String, target :: a }
      | Binary  { lhs :: a, op :: String, rhs :: a }
      | Call    { func :: a, args :: [a] }
      | Paren   { target :: a }
      deriving (Show, Eq, Functor)


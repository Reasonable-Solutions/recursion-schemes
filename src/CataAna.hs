{-# LANGUAGE DeriveFunctor #-}
module CataAna
where
import           Text.PrettyPrint               ( Doc )
import qualified Text.PrettyPrint              as P
import           Data.Semigroup                 ( (<>) )
import           Data.Foldable                  ( )
import           Control.Arrow                  ( (>>>)
                                                , (<<<)
                                                )
data Expr a
      = Literal { intVal :: Int}
      | Ident   { name :: String}
      | Index   { target :: a, idx :: a }
      | Unary   { op :: String, target :: a }
      | Binary  { lhs :: a, op :: String, rhs :: a }
      | Call    { func :: a, args :: [a] }
      | Paren   { target :: a }
      deriving (Show, Eq, Functor)

-- | this here is really fix, `Fix f = f (Fix f)`
newtype Term f = In {out :: f (Term f)} -- ^ this is just fix, really

-- | an algebra is just `f a -> a`
type Algebra f a = f a -> a -- ^ like this

type CoAlgebra f a = a -> f a

prettyPrint :: Algebra Expr Doc -- Expr Doc -> Doc
prettyPrint (Literal i     ) = P.int i
prettyPrint (Ident   s     ) = P.text s
prettyPrint (Index it index) = it <> P.brackets index
prettyPrint (Unary op it   ) = P.text op <> it
prettyPrint (Binary l op r ) = l <> P.text op <> r
prettyPrint (Call fn args) =
      fn <> P.parens (P.cat (P.punctuate (P.text ", ") args))
prettyPrint (Paren exp) = P.parens exp

ten, add, call :: Term Expr
ten = In Literal {intVal = 10}
add = In Ident {name = "add"}
call = In Call {func = add, args = [ten, ten]}

-- | this is really just `cata` specialized to `Term`s
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn

-- | this is just really `ana` specialized to 'Term`s
topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown fn = In <<< fmap (topDown fn) <<< out <<< fn

cata :: Functor f => Algebra f a -> (Term f -> a)
cata fn = -- this could be pointfree
      out  -- unpack 
            >>> fmap (cata fn)  -- recurse
            >>> fn -- apply

ana :: Functor f => CoAlgebra f a -> (a -> Term f)
ana fn = --this could be pointfree too, and <<< === .
      In -- pack! 
            <<< fmap (ana fn) -- recurse
            <<< fn -- apply

countNodes :: Algebra Expr Int
countNodes (Unary _ arg        ) = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call  fn args      ) = fn + sum args + 1
countNodes (Index it idx       ) = it + idx + 1
countNodes (Paren   arg        ) = arg + 1
countNodes (Literal _          ) = 1
countNodes (Ident   _          ) = 1


























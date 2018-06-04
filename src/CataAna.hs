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
import           Lib

-- | this here is really fix, `Fix f = f (Fix f)`
newtype Term f = In {out :: f (Term f)} -- ^ this is just fix, really

-- | I mean, litterally, this fix.
newtype Fix f = Fix (f (Fix f))

-- | with the traditional unfix == out
unFix :: Fix f -> f (Fix f)
unFix (Fix f) = f

-- | an algebra is just `f a -> a`
type Algebra f a = f a -> a -- ^ like this

-- | a CoAlgebra is just a __co__ - Algebra
type CoAlgebra f a = a -> f a

 {-/ 
 a catamorphisms takes an algebra (arbitrary!) and returns a 
 function that knows how to evaluate a recursive structure (by way of fix)
 down to just a value `a` 
 -}
cata :: Functor f => Algebra f a -> (Term f -> a)
cata fn = -- this could be pointfree with >>> === flip .
      out  -- unpack 
            >>> fmap (cata fn)  -- recurse
            >>> fn -- apply

ana :: Functor f => CoAlgebra f a -> (a -> Term f)
ana fn = --this could be pointfree too, and <<< === .
      In -- pack! 
            . fmap (ana fn) -- recurse
            . fn -- apply

-- this is, in other words, a simple algebra over Expr where the carrier type is Int
countNodes :: Algebra Expr Int  
countNodes (Unary _ arg        ) = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call  fn args      ) = fn + sum args + 1
countNodes (Index it idx       ) = it + idx + 1
countNodes (Paren   arg        ) = arg + 1
countNodes (Literal _          ) = 1
countNodes (Ident   _          ) = 1

-- | the carrier type can be even more fancy things
prettyPrint :: Algebra Expr Doc -- Expr Doc -> Doc
prettyPrint (Literal i     ) = P.int i
prettyPrint (Ident   s     ) = P.text s
prettyPrint (Index it index) = it <> P.brackets index
prettyPrint (Unary op it   ) = P.text op <> it
prettyPrint (Binary l op r ) = l <> P.text op <> r
prettyPrint (Call fn args  ) = fn <> P.parens (P.cat (P.punctuate (P.text ", ") args))
prettyPrint (Paren exp     ) = P.parens exp

-- evaluate :: Algebra Expr Int  -- Expre Int -> Int
-- evaluate (Literal i) = i
-- evaluate (Ident s) = s
-- evaluate (Index it index) = it !! index
-- evaluate (Unary op it) = op it
-- evaluate (Binary l op r) = l op r
-- evaluate (Call fn args) = fn args
-- evaluate (Paren exp) = exp

ten, add, call :: Term Expr
ten = In Literal {intVal = 10}
add = In Ident {name = "add"}
call = In Call {func = add, args = [ten, ten]}


-- e.g ``` cata prettyPrint call ```

-- | this is really just `cata` specialized to `Term`s
bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn

-- | this is just really `ana` specialized to 'Term`s
topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown fn = In <<< fmap (topDown fn) <<< out <<< fn




























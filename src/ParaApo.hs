module ParaApo
where
import           Lib
import           Data.Function                  ( (&) )
import           Control.Arrow                  ( (<<<)
                                                , (>>>)
                                                , (|||)
                                                , (&&&)
                                                )

-- | Still fix, tho
newtype Term f = In {out :: f (Term f)}

-- | I guess R is for right, as in this ain't commutative
type RAlgebra f a = f (Term f, a) -> a -- ^ the "tupling trick!"
type RCoAlgebra f a = a -> f (Either (Term f) a) -- ^ Either is a dual of (,)
type Algebra f a = f a -> a

-- | but, a tuple is just a product, just like (-> a b)
type RAlgebra' f a = Term f -> f a -> a

-- | tuple to function paramorphism
para' :: Functor f => RAlgebra' f a -> (Term f -> a)
para' alg t = out t & fmap (para' alg) & alg t

cata' :: Functor f => Algebra f a -> (Term f -> a)
cata' f = para' (const f)

-- | The main result, imo
para :: (Functor f) => RAlgebra f a -> (Term f -> a)
para f = out >>> fmap (id &&& para f) >>> f

-- | the other main result, imo
apo :: (Functor f) => RCoAlgebra f a -> (a -> Term f)
apo f = In <<< fmap (id ||| apo f) <<< f

fanin :: Functor f => RCoAlgebra f a -> Either (Term f) a -> Term f
fanin f = either id (apo f)













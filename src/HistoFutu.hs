{-# LANGUAGE DeriveFunctor #-}
module HistoFutu where
import           Data.Semigroup                 ( (<>) )
import           Data.Foldable                  ( )
import           Control.Arrow                  ( (>>>)
                                                , (<<<)
                                                )
import           Lib

newtype Fix f = In {out :: f (Fix f) }


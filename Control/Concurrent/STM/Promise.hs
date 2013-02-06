{-# LANGUAGE DeriveFunctor #-}
module Control.Concurrent.STM.Promise
    ( Promise(..)
    , PromiseResult(..)
    , isAn, isUnfinished, isCancelled
    , eitherResult, bothResults, bothResultsSemigroup
    ) where

import Control.Monad.STM
import Data.Semigroup

data Promise a = Promise
    { spawn  :: IO ()
    , cancel :: IO ()
    , result :: STM (PromiseResult a)
    }
  deriving Functor

data PromiseResult a
    = Unfinished
    | Cancelled
    | An a
  deriving (Functor, Eq, Ord, Show)

an :: PromiseResult a -> a
an (An a) = a
an _      = error "an: on non-An result!"

isAn :: PromiseResult a -> Bool
isAn An{} = True
isAn _    = False

isUnfinished :: PromiseResult a -> Bool
isUnfinished Unfinished{} = True
isUnfinished _            = False

isCancelled :: PromiseResult a -> Bool
isCancelled Cancelled{} = True
isCancelled _           = False

eitherResult :: PromiseResult a -> PromiseResult a -> PromiseResult a
eitherResult (An a)     _          = An a
eitherResult _          (An a)     = An a
eitherResult Unfinished _          = Unfinished
eitherResult _          Unfinished = Unfinished
eitherResult _          _          = Cancelled

bothResults :: PromiseResult a -> PromiseResult a -> PromiseResult (a,a)
bothResults (An a)    (An e)    = An (a,e)
bothResults Cancelled _         = Cancelled
bothResults _         Cancelled = Cancelled
bothResults _         _         = Unfinished

bothResultsSemigroup :: Semigroup a =>
                        PromiseResult a -> PromiseResult a -> PromiseResult a
bothResultsSemigroup a b = fmap (uncurry (<>)) (bothResults a b)


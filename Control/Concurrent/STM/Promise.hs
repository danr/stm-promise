{-# LANGUAGE DeriveFunctor #-}
module Control.Concurrent.STM.Promise
    ( Promise(..), an
    , PromiseResult(..)
    , isAn, isUnfinished, isCancelled
    , eitherResult, bothResults, manyResults
    ) where

import Control.Monad.STM

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
eitherResult _          (An e)     = An e
eitherResult Unfinished _          = Unfinished
eitherResult _          Unfinished = Unfinished
eitherResult _          _          = Cancelled

bothResults :: PromiseResult a -> PromiseResult a -> PromiseResult (a,a)
bothResults (An a)    (An e)    = An (a,e)
bothResults Cancelled _         = Cancelled
bothResults _         Cancelled = Cancelled
bothResults _         _         = Unfinished

manyResults :: PromiseResult a -> PromiseResult a -> PromiseResult (Either a (a,a))
manyResults (An a)     (An e)     = An $ Right (a,e)
manyResults (An a)     _          = An $ Left a
manyResults _          (An e)     = An $ Left e
manyResults Unfinished _          = Unfinished
manyResults _          Unfinished = Unfinished
manyResults _          _          = Cancelled


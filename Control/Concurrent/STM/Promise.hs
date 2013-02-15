{-# LANGUAGE DeriveFunctor #-}
-- | Promises that allow spawning and cancelling in `IO`, and an `STM` result
module Control.Concurrent.STM.Promise
    (
    -- * Promises
       Promise(..), an,
    -- * Results
       PromiseResult(..),
    -- ** Querying results
       isAn, isUnfinished, isCancelled,
    -- ** Combining results
       eitherResult, eitherResult', bothResults, bothResults'
    ) where

import Control.Monad.STM
import Data.Monoid

-- | A promise
data Promise a = Promise
    { spawn  :: IO ()
    -- ^ Instruction for spawning
    , cancel :: IO ()
    -- ^ Instruction for cancelling
    , result :: STM (PromiseResult a)
    -- ^ The result of a computation
    }
  deriving Functor

-- | The result of the promise
data PromiseResult a
    = Unfinished
    -- ^ Not finished yet (or not even spawned yet))
    | Cancelled
    -- ^ Cancelled
    | An a
    -- ^ A result
  deriving (Functor, Eq, Ord, Show)

-- | Gets the result (partial function)
an :: PromiseResult a -> a
an (An a) = a
an _      = error "an: on non-An result!"

-- | Is this a result?
isAn :: PromiseResult a -> Bool
isAn An{} = True
isAn _    = False

-- | Is this unfinished?
isUnfinished :: PromiseResult a -> Bool
isUnfinished Unfinished{} = True
isUnfinished _            = False

-- | Is this cancelled?
isCancelled :: PromiseResult a -> Bool
isCancelled Cancelled{} = True
isCancelled _           = False

-- Possible tests:
-- Check that the primed versions are commutative, given a commutative monoid.

-- | If either is finished (`An`), return one of them (favor the first one)
--
--   If either is `Unfinished`, this is also `Unfinished`.
--
--   Otherwise, both are `Cancelled` and so is this.
eitherResult :: PromiseResult a -> PromiseResult a -> PromiseResult a
eitherResult (An a)     _          = An a
eitherResult _          (An e)     = An e
eitherResult Unfinished _          = Unfinished
eitherResult _          Unfinished = Unfinished
eitherResult _          _          = Cancelled

-- | As `eitherResult`, but upon only one returned failure, waits for the other, and then
--   either `mappend`s the results or returns the failure if it is `Cancelled`.
--
--   `mempty` is not used.
eitherResult' :: Monoid a => (a -> Bool) -> PromiseResult a -> PromiseResult a -> PromiseResult a
eitherResult' failure (An a)     b          | failure a = case b of An e       -> An (a `mappend` e)
                                                                    Unfinished -> Unfinished
                                                                    Cancelled  -> An a
                                            | otherwise = An a
eitherResult' failure a          (An e)     | failure e = case a of An i       -> An (i `mappend` e)
                                                                    Unfinished -> Unfinished
                                                                    Cancelled  -> An e
                                            | otherwise = An e
eitherResult' _       Unfinished _          = Unfinished
eitherResult' _       _          Unfinished = Unfinished
eitherResult' _       _          _          = Cancelled

-- | If both are finished (`An`), return them in a tuple.
--
--   If either is `Cancelled`, this is also `Cancelled`.
--
--   Otherwise, both are `Unfinished` and so is this.
bothResults :: PromiseResult a -> PromiseResult b -> PromiseResult (a,b)
bothResults (An a)    (An e)    = An (a,e)
bothResults Cancelled _         = Cancelled
bothResults _         Cancelled = Cancelled
bothResults _         _         = Unfinished

-- | Prefers to return failures than Cancelled
--
--   `mempty` is not used.
bothResults' :: Monoid a => (a -> Bool) -> PromiseResult a -> PromiseResult a -> PromiseResult a
bothResults' _       (An a)    (An e)    = An (a `mappend` e)
bothResults' failure (An a)    _         | failure a = An a
bothResults' failure _         (An e)    | failure e = An e
bothResults' _       Cancelled _         = Cancelled
bothResults' _       _         Cancelled = Cancelled
bothResults' _       _         _         = Unfinished


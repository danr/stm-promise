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
       eitherResult, bothResults
    ) where

import Control.Monad.STM

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


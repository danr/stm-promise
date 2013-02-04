{-# LANGUAGE DeriveFunctor #-}
module Control.Concurrent.STM.Promise
    ( Promise(..)
    , PromiseResult(..)
    , isAn, isUnfinished, isCancelled
    , anyResult, allResults
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
    | An { an :: a }
  deriving (Functor, Eq, Ord, Show)

isAn :: PromiseResult a -> Bool
isAn An{} = True
isAn _    = False

isUnfinished :: PromiseResult a -> Bool
isUnfinished Unfinished{} = True
isUnfinished _            = False

isCancelled :: PromiseResult a -> Bool
isCancelled Cancelled{} = True
isCancelled _           = False

anyResult :: [PromiseResult a] -> PromiseResult a
anyResult rs
    | r:_ <- filter isAn rs = r
    | all isCancelled rs    = Cancelled
    | otherwise             = Unfinished

allResults :: [PromiseResult a] -> PromiseResult [a]
allResults rs
    | any isCancelled  rs = Cancelled
    | any isUnfinished rs = Unfinished
    | otherwise           = An (map an rs)


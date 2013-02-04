{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
module Control.Concurrent.STM.Promise.Tree where

import Control.Concurrent
import Control.Concurrent.STM.Promise
import Control.Monad
import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Monoid
import Data.Typeable

-- Labelled trees
data Tree a = And [Tree a] | Or [Tree a] | Leaf a
  deriving (Eq, Ord, Show, Typeable, Functor)

spawnTree :: Tree (Promise a) -> IO ()
spawnTree (And ps) = mapM_ spawnTree ps
spawnTree (Or ps)  = mapM_ spawnTree ps
spawnTree (Leaf a) = spawn a

cancelTree :: Tree (Promise a) -> IO ()
cancelTree (And ps) = mapM_ cancelTree ps
cancelTree (Or ps)  = mapM_ cancelTree ps
cancelTree (Leaf a) = cancel a

unleaf :: [Tree a] -> Maybe [a]
unleaf []          = Just []
unleaf (Leaf x:xs) = (x:) <$> unleaf xs
unleaf _           = Nothing

promises :: Tree (Promise a) -> [Promise a]
promises (Leaf a) = [a]
promises (And ts) = concatMap promises ts
promises (Or ts)  = concatMap promises ts

-- the mysterious TVar Integer is an experiment to watch changes in the nodes further down... hmm
-- maybe watchTree should return this integer instead, and we could watch for changes on it instead.
-- the goal is to be able to watch this tree as it updates. that would be nice
watchTree :: Monoid a => TVar Integer -> Tree (Promise a) -> IO (TVar (Tree (PromiseResult a)))
watchTree ws t = case t of

    Leaf a ->
        forkWrapTVar ws $ \ write -> atomically $ do
            r <- result a
            when (isUnfinished r) retry
            write r

    And ts -> watchTrees (fmap mconcat . allResults) ts

    Or ts -> watchTrees anyResult ts

  where

    watchTrees k ts = do
        ts' <- mapM (watchTree ws) ts
        forkWrapTVar ws $ \ write -> do
            atomically $ do

                let interpret Nothing   = Unfinished
                    interpret (Just rs) = k rs

                r <- interpret . unleaf <$> mapM readTVar ts'
                -- I would like to write AND retry here
                when (isUnfinished r) retry
                write r

            cancelTree t


forkWrapTVar :: TVar Integer -> ((PromiseResult a -> STM ()) -> IO ()) -> IO (TVar (Tree (PromiseResult a)))
forkWrapTVar w mk = do
    v <- newTVarIO (Leaf Unfinished)
    forkIO $ mk $ \ x -> do
        writeTVar v (Leaf x)
        modifyTVar w succ
    return v


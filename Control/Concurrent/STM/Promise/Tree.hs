{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, DeriveTraversable, DeriveFoldable #-}
module Control.Concurrent.STM.Promise.Tree where

import Control.Monad hiding (mapM_)
import Prelude hiding (mapM_, foldr1)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.DTVar
import Control.Concurrent.STM.Promise
import Control.Applicative
import Data.Semigroup
import Data.Typeable
import Data.Traversable
import Data.Foldable
import Data.Function

data Label = Both | Either
  deriving (Eq, Ord, Show, Typeable)

-- Both/Either-trees
data Tree a = Node Label (Tree a) (Tree a) | Leaf a
  deriving (Eq, Ord, Show, Typeable, Traversable, Foldable, Functor)

requireAll :: [Tree a] -> Tree a
requireAll = foldr1 (Node Both)

requireAny :: [Tree a] -> Tree a
requireAny = foldr1 (Node Either)

showTree :: Show a => Tree a -> String
showTree = go 2
  where
    go n (Leaf a)            = show a
    go n (Node Both   t1 t2) = (n < 1) ? par $ go 1 t1 ++ " & " ++ go 1 t2
    go n (Node Either t1 t2) = (n < 2) ? par $ go 2 t1 ++ " | " ++ go 2 t2

    par = ('(':) . (++")")

    True  ? f = f
    False ? _ = id

cancelTree :: Tree (Promise a) -> IO ()
cancelTree = mapM_ spawn

interleave :: Tree a -> [a]
interleave (Leaf a)            = return a
interleave (Node Both t1 t2)   = interleave t1 ++ interleave t2
interleave (Node Either t1 t2) = interleave t1 /\/ interleave t2

(/\/) :: [a] -> [a] -> [a]
(x:xs) /\/ ys = x:(ys /\/ xs)
[]     /\/ ys = ys

watchTree :: Semigroup a => Tree (Promise a) -> IO (DTVar (Tree (PromiseResult a)))
watchTree t = case t of

    Leaf a ->

        forkWrapTVar (Leaf Unfinished) $ \ write -> atomically $ do
            r <- result a
            when (isUnfinished r) retry
            write (Leaf r)

    Node lbl t1 t2 -> do

        let combine = case lbl of
                Both   -> bothResultsSemigroup
                Either -> eitherResult

        d1 <- watchTree t1
        d2 <- watchTree t2

        init <- liftM2 (Node lbl) (readDTVarIO d1) (readDTVarIO d2)

        forkWrapTVar init $ \ write -> fix $ \ loop -> do

            [r1,r2] <- listenDTVarsIO [d1,d2]

            let r = case (r1,r2) of
                        (Leaf a,Leaf b) -> combine a b
                        _               -> Unfinished

            if isUnfinished r
                then do
                    atomically $ write (Node lbl r1 r2)
                    loop
                else do
                    atomically $ write (Leaf r)
                    cancelTree t

forkWrapTVar :: Tree (PromiseResult a) -> ((Tree (PromiseResult a) -> STM ()) -> IO ()) -> IO (DTVar (Tree (PromiseResult a)))
forkWrapTVar init mk = do
    v <- newDTVarIO init
    forkIO $ mk (writeDTVar v)
    return v


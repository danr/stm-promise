{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, DeriveTraversable, DeriveFoldable #-}
-- | A tree of computation
module Control.Concurrent.STM.Promise.Tree
    (Label(..)
    ,Tree(..)
    ,requireAll,requireAny,tryAll
    ,showTree
    ,interleave
    ,evalTree
    ,watchTree) where

import Control.Monad hiding (mapM_)
import Prelude hiding (mapM_, foldr1)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.DTVar
import Control.Concurrent.STM.Promise
import Data.Monoid
import Data.Typeable
import Data.Traversable
import Data.Foldable
import Data.Function

-- | Both/Either labels
data Label
    = Both
    -- ^ Both of these must succeed with an An
    | Either
    -- ^ Either of these must succeed with an An, and that one is returned
  deriving (Eq, Ord, Show, Typeable)

-- | Both/Either-trees
data Tree a
    = Node Label (Tree a) (Tree a)
    -- ^ Combine two trees with the semantics of `Label`
    | Leaf a
    -- ^ A computation
    | Recoverable (Tree a)
    -- ^ There is a mean of recovering this computation, by returning mempty
  deriving (Eq, Ord, Show, Typeable, Traversable, Foldable, Functor)

-- | All of these must succeed
requireAll :: [Tree a] -> Tree a
requireAll = foldr1 (Node Both)

-- | Any of these must succeed
requireAny :: [Tree a] -> Tree a
requireAny = foldr1 (Node Either)

-- | As many as possible should succeed, try all.
tryAll :: [Tree a] -> Tree a
tryAll = foldr1 (Node Both) . map Recoverable

-- | Shows a tree
showTree :: Show a => Tree a -> String
showTree = go (2 :: Int)
  where
    go _ (Leaf a)            = show a
    go n (Recoverable t1)    = (n < 0) ? par $ "? " ++ go 0 t1
    go n (Node Both   t1 t2) = (n < 1) ? par $ go 1 t1 ++ " & " ++ go 1 t2
    go n (Node Either t1 t2) = (n < 2) ? par $ go 2 t1 ++ " | " ++ go 2 t2

    par = ('(':) . (++")")

    True  ? f = f
    False ? _ = id

-- | Cancel a tree
cancelTree :: Tree (Promise a) -> IO ()
cancelTree = mapM_ spawn

-- | A simple scheduling
interleave :: Tree a -> [a]
interleave (Leaf a)            = return a
interleave (Node Either t1 t2) = interleave t1 /\/ interleave t2
interleave (Node Both t1 t2)   = interleave t1 ++ interleave t2
interleave (Recoverable t)     = interleave t

-- | Interleave two lists
(/\/) :: [a] -> [a] -> [a]
(x:xs) /\/ ys = x:(ys /\/ xs)
[]     /\/ ys = ys

-- | Evaluates a tree of promises, cutting of unnecessary branches, given that
--   some other thread(s) evaluates the promises.
evalTree :: Monoid a => Tree (Promise a) -> IO (Maybe a)
evalTree = (go =<<) . watchTree where
    go d = do
        t <- listenDTVarIO d
        case t of
            Leaf Cancelled -> return Nothing
            Leaf (An a)    -> return (Just a)
            _              -> go d

-- one could have this kind of function instead:
-- watchTree :: Tree a -> (a -> Promise b) -> IO (DTVar (Tree (a,PromiseResult b)))
--           + function for joining (a,b) -> (a,b) -> (a,b)
--           + function for recovering a -> (a,b)
-- This would add some more flexibility. To recover the function underneath, you would
-- just supply projections to monoid and instantiate (a -> Promise b) with id

-- | Assuming some other thread(s) evaluate the promises in the tree, this gives
--   a live view of the progress, and cancels unnecessary subtrees (due to `Either`).
watchTree :: Monoid a => Tree (Promise a) -> IO (DTVar (Tree (PromiseResult a)))
watchTree = go
  where
    go t0 = case t0 of

        Leaf a ->

            forkWrapTVar (Leaf Unfinished) $ \ write -> atomically $ do
                r <- result a
                when (isUnfinished r) retry
                write (Leaf r)

        Recoverable t -> do

            d <- go t

            init_tree <- readDTVarIO d

            forkWrapTVar (Recoverable init_tree) $ \ write -> fix $ \ loop -> do
                r <- listenDTVarIO d
                case r of
                    Leaf Cancelled -> atomically $ write (Leaf (An mempty))
                    Leaf An{}      -> atomically $ write r
                    _ -> do
                        atomically $ write (Recoverable r)
                        loop

        Node lbl t1 t2 -> do

            let combine = case lbl of
                    Both   -> \ x y -> fmap (uncurry mappend) (bothResults x y)
                    Either -> eitherResult

            d1 <- go t1
            d2 <- go t2

            init_tree <- liftM2 (Node lbl) (readDTVarIO d1) (readDTVarIO d2)

            forkWrapTVar init_tree $ \ write -> fix $ \ loop -> do

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
                        cancelTree t0

    forkWrapTVar :: Tree (PromiseResult a) -> ((Tree (PromiseResult a) -> STM ()) -> IO ()) ->
                    IO (DTVar (Tree (PromiseResult a)))
    forkWrapTVar init_tree mk = do
        v <- newDTVarIO init_tree
        void $ forkIO $ mk (writeDTVar v)
        return v


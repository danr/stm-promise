{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, DeriveTraversable, DeriveFoldable #-}
-- | A tree of computation
module Control.Concurrent.STM.Promise.Tree
    (
    -- * Trees
       Tree(..), Label(..),
    -- * Creating trees
       requireAll,requireAny,tryAll,
    -- * Evaluating trees
       evalTree,watchTree,
    -- * Scheduling
       interleave,
    -- * Utilities
       showTree
    ) where

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

-- The free monad over the underlying structure
instance Monad Tree where
    return              = Leaf
    Leaf x >>= f        = f x
    Node l u v >>= f    = Node l (u >>= f) (v >>= f)
    Recoverable t >>= f = Recoverable (t >>= f)

-- | Passes a list along if it is nonempty, otherwise raises an error message
ensureNonempty :: String -> [a] -> [a]
ensureNonempty s [] = error $ s ++ ": non-empty list!"
ensureNonempty _ xs = xs

-- | All of these must succeed
requireAll :: [Tree a] -> Tree a
requireAll = foldr1 (Node Both) . ensureNonempty "requireAll"

-- | Any of these must succeed
requireAny :: [Tree a] -> Tree a
requireAny = foldr1 (Node Either) . ensureNonempty "requireAny"

-- | As many as possible should succeed, try all.
tryAll :: [Tree a] -> Tree a
tryAll = foldr1 (Node Both) . map Recoverable . ensureNonempty "tryAll"

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
cancelTree = mapM_ cancel

-- | A simple scheduling (see `Control.Concurrent.STM.Promise.Workers.workers`)
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
--
--   The first argument is the same as for `watchTree`. Is currently
--   implemented in terms of `watchTree`, rather than something more efficient
--   (such as `TMVars`).
--
--   The first result is the failures, then the actual result
evalTree :: Monoid a => (a -> Bool) -> Tree (Promise a) -> IO (a,a)
evalTree failure t0 = do
    (err_chan,dtvar) <- watchTree failure t0
    res_val <- go dtvar
    res_err <- go' err_chan
    return (res_err,res_val)
  where
    go d = do
        t <- listenDTVarIO d
        case t of
            Leaf Cancelled -> return mempty
            Leaf (An a)    -> return a
            _              -> go d

    go' ch = do
        e <- atomically (tryReadTChan ch)
        case e of
            Nothing -> return mempty
            Just x  -> mappend x `liftM` go' ch

-- | Assuming some other thread(s) evaluate the promises in the tree, this gives
--   a live view of the progress, and cancels unnecessary subtrees (due to `Either`).
--
--   The first argument is a way to deem promises with results as failures. `(== mempty)` or
--   (const False) could be good alternatives. These failures are sent to the TChan.
watchTree :: Monoid a => (a -> Bool) -> Tree (Promise a) -> IO (TChan a,DTVar (Tree (PromiseResult a)))
watchTree failure t_init = do
    ch <- newTChanIO
    (,) ch `liftM` go_intermediate ch t_init
  where
    go_intermediate failure_chan = go
      where
        go t0 = case t0 of

            Leaf u ->

                forkWrapTVar (Leaf Unfinished) $ \ write -> atomically $ do
                    r <- result u
                    when (isUnfinished r) retry
                    write . Leaf =<< case r of
                        An a | failure a -> do
                            writeTChan failure_chan a
                            return (An mempty)
                        _ -> return r

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

{- one could have this kind of function instead:
   watchTree :: Tree a -> (a -> Promise b) -> IO (DTVar (Tree (a,PromiseResult b)))
             + function for joining (a,b) -> (a,b) -> (a,b)
             + function for recovering a -> (a,b)
   This would add some more flexibility. To recover the function underneath, you would
   just supply projections to monoid and instantiate (a -> Promise b) with id
-}

{-
    Why don't I just return Tree (PromiseResult a) ?
    This library seriously needs to be rethought
-}

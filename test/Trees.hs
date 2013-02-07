{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ViewPatterns #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.DTVar
import Control.Concurrent.STM.Promise
import Control.Concurrent.STM.Promise.Tree
import Control.Concurrent.STM.Promise.Workers
import Control.Monad
import Data.List
import Data.Function
import Data.IORef
import Data.Monoid
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Test

nubSorted :: Ord a => [a] -> [a]
nubSorted = map head . group . sort

newtype Desc a = Desc [a]
  deriving (Eq,Ord,Monoid)

instance Show a => Show (Desc a) where
    show (Desc as) = show as

instance Arbitrary a => Arbitrary (Desc a) where
    arbitrary = Desc . return <$> arbitrary

instance Monoid Int where
    mempty  = 0
    mappend = (+)

eval :: (Ord a,Monoid a) => Tree a -> [a]
eval = go where
    go t0 = case t0 of
        Leaf x            -> return x
        Node Both t1 t2   -> nubSorted $ mappend <$> go t1 <*> go t2
        Node Either t1 t2 -> nubSorted $ go t1 ++ go t2
        Recoverable t     -> insert mempty (go t)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree where
        arbTree ((`div` 2) -> s) = frequency
            [(1,Leaf <$> arbitrary)
            ,(s,Node Both   <$> arbTree s <*> arbTree s)
            ,(s,Node Either <$> arbTree s <*> arbTree s)
            ,(s`div`2,attempts    <$> arbTree s <*> arbTree s)
            ,(s`div`4,Recoverable <$> arbTree (s-1))
            ]
          where
            attempts u v = Node Both (Recoverable u) (Recoverable v)


delayPromise :: a -> Int -> IO (Promise a)
delayPromise b t = do

    res_var <- newTVarIO Unfinished

    let spawn = do
            threadDelay t
            atomically $ do
                res <- readTVar res_var
                case res of
                    Unfinished -> writeTVar res_var (An b)
                    _          -> return ()

        cancel = atomically $ writeTVar res_var Cancelled

        result = readTVar res_var

    return Promise{..}

mkPromiseTree :: Arbitrary a => Int -> Tree a -> Gen (IO (Tree (Promise a)))
mkPromiseTree timeout = go where
    go t = case t of
        Leaf b -> do
            to <- choose (0,timeout * 2)
            return $ Leaf <$> delayPromise b to
        Node lbl t1 t2 -> liftM2 (Node lbl) <$> go t1 <*> go t2
        Recoverable t' -> liftM Recoverable <$> go t'

prop_equal :: (Show a,Ord a,Arbitrary a,Monoid a) =>
              a -> IO () -> IO () -> Int -> Int -> Tree a -> Property
prop_equal _ add_test add_cancelled cores timeout tree = do
    io_promise_tree <- mkPromiseTree timeout tree
    monadicIO $ assert <=< run $ do
        putStrLn "== New test =="
        putStrLn (showTree tree)
        putStrLn $ "queue order: " ++ show (interleave tree)
        putStrLn $ "evaluations: " ++ show (eval tree)
        promise_tree <- io_promise_tree
        workers (Just timeout) cores (interleave promise_tree)
        tree_dtvar <- watchTree promise_tree
        fix $ \ loop -> do
            t <- listenDTVarIO tree_dtvar
            putStrLn (showTree t)
            case t of
                Leaf Unfinished -> loop
                Leaf (An b) -> add_test >> return (b `elem` eval tree)
                Leaf Cancelled -> add_test >> add_cancelled >> return True
                _ -> loop

runTest :: (Show a,Ord a,Arbitrary a,Monoid a) => a -> Int -> IO ((Int,Int),Bool)
runTest a size = do
    cancelled <- newIORef (0 :: Int)
    tests <- newIORef (0 :: Int)
    res <- quickCheckWithResult stdArgs { maxSuccess = 1000, maxSize = size }
        (prop_equal a (modifyIORef tests succ) (modifyIORef cancelled succ) 10 10000)
    ts <- readIORef tests
    cs <- readIORef cancelled
    return ((ts,cs),isSuccess res)


main :: IO ()
main = do
    (times,tests) <- unzip <$> sequence
        [ runTest (undefined :: Desc Int) 25
        , runTest (undefined :: Int) 50
        ]
    forM_ times $ \(ts,cs) -> putStrLn $ show ts ++ " tests, " ++ show cs ++ " cancelled."
    unless (and tests) exitFailure


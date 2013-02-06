{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell, RecordWildCards #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.DTVar
import Control.Concurrent.STM.Promise
import Control.Concurrent.STM.Promise.Tree
import Control.Monad
import Data.List
import Data.Ord
import Data.Function
import Data.IORef
import Data.Semigroup
import System.Exit
import Test.Feat
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Test

nubSorted :: Ord a => [a] -> [a]
nubSorted = map head . group . sort

expensiveConfig :: WatchConfig [[a]]
expensiveConfig = WatchConfig
    { combine_both = \ xs ys -> (++) <$> xs <*> ys
    , combine_many = either id (uncurry (++))
    }

simpleConfig :: WatchConfig Int
simpleConfig = WatchConfig
    { combine_both = min
    , combine_many = either id (uncurry max)
    }

eval :: Ord a => WatchConfig a -> Tree a -> [a]
eval WatchConfig{..} = go where
    go t = case t of
        Leaf x            -> return x
        Node Both t1 t2   -> nubSorted $ combine_both <$> go t1 <*> go t2
        Node Either t1 t2 -> go t1 `union` go t2
        Node Many t1 t2   -> nubSorted $ do
            e1 <- go t1
            e2 <- go t2
            u <- [Left e1,Left e2,Right (e1,e2)]
            return $ combine_many u

deriveEnumerable ''Label
deriveEnumerable ''Tree

instance Enumerable a => Arbitrary (Tree a) where
    arbitrary = sized uniform

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

maybeIO :: Maybe a -> (a -> IO b) -> IO (Maybe b)
maybeIO m f = maybe (return Nothing) (fmap Just . f) m

worker :: Maybe Int -> TChan (Promise a) -> IO ()
worker m_t ch = fix $ \ loop -> do
    m_promise <- atomically $ tryReadTChan ch
    case m_promise of
        Nothing -> return ()
        Just promise -> do
            m_thr <- maybeIO m_t $ \ timeout -> forkIO $ do
                threadDelay timeout
                cancel promise

            spawn promise

            atomically $ do
                status <- result promise
                when (isUnfinished status) retry

            void $ maybeIO m_thr killThread

            loop

-- Evaluate these promises on n processors, using maybe a timeout in
-- microseconds
workers :: Maybe Int -> Int -> [Promise a] -> IO ()
workers m_t n xs = do
    ch <- newTChanIO
    atomically $ mapM_ (writeTChan ch) xs
    replicateM_ n $ forkIO $ worker m_t ch

mkPromiseTree :: Arbitrary a => Int -> Tree a -> Gen (IO (Tree (Promise a)))
mkPromiseTree timeout = go where
    go t = case t of
        Leaf b -> do
            to <- choose (0,timeout * 2)
            return $ Leaf <$> delayPromise b to
        Node lbl t1 t2 -> liftM2 (Node lbl) <$> go t1 <*> go t2

prop_equal :: (Enumerable a,Show a,Ord a,Arbitrary a) =>
              WatchConfig a -> IO () -> IO () -> Int -> Int -> Tree a -> Property
prop_equal config add_test add_cancelled cores timeout tree = do
    io_promise_tree <- mkPromiseTree timeout tree
    monadicIO $ assert <=< run $ do
        putStrLn "== New test =="
        putStrLn (showTree tree)
        putStrLn $ "queue order: " ++ show (interleave tree)
        putStrLn $ "evaluations: " ++ show (eval config tree)
        promise_tree <- io_promise_tree
        workers (Just timeout) cores (interleave promise_tree)
        tree_dtvar <- watchTree config promise_tree
        fix $ \ loop -> do
            t <- listenDTVarIO tree_dtvar
            -- putStrLn (showTree t)
            case t of
                Leaf Unfinished -> loop
                Leaf (An b) -> add_test >> return (b `elem` eval config tree)
                Leaf Cancelled -> add_test >> add_cancelled >> return True
                _ -> loop

runTest :: (Enumerable a,Show a,Ord a,Arbitrary a) =>
           Int -> WatchConfig a -> IO Bool
runTest size config = do
    cancelled <- newIORef (0 :: Int)
    tests <- newIORef (0 :: Int)
    res <- quickCheckWithResult stdArgs { maxSuccess = size, maxSize = size }
        (prop_equal config (modifyIORef tests succ) (modifyIORef cancelled succ) 10 10000)
    ts <- readIORef tests
    cs <- readIORef cancelled
    putStrLn $ show ts ++ " tests, " ++ show cs ++ " cancelled."
    return $ isSuccess res


main :: IO ()
main = do
    tests <- sequence $
        [ runTest 200 (expensiveConfig :: WatchConfig [[Int]])
        , runTest 500 (simpleConfig :: WatchConfig Int)
        ]
    unless (and tests) exitFailure


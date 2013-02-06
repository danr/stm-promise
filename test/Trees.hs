{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell, RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

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
import Data.Typeable
import Data.Monoid
import System.Exit
import Test.Feat
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Test

nubSorted :: Ord a => [a] -> [a]
nubSorted = map head . group . sort

newtype Desc a = Desc [[a]]
  deriving (Eq,Ord,Show,Arbitrary,Enumerable,Typeable)

instance Monoid (Desc a) where
    mempty                        = Desc $ [[]]
    mappend (Desc xss) (Desc yss) = Desc $ (++) <$> xss <*> yss

instance Monoid Int where
    mempty  = 0
    mappend = (+)

eval :: (Ord a,Monoid a) => Tree a -> [a]
eval = go where
    go t = case t of
        Leaf x            -> return x
        Node Both t1 t2   -> nubSorted $ mappend <$> go t1 <*> go t2
        Node Either t1 t2 -> nubSorted $ go t1 ++ go t2
        Recoverable t     -> insert mempty (go t)

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
        Recoverable t' -> liftM Recoverable <$> go t'

prop_equal :: (Enumerable a,Show a,Ord a,Arbitrary a,Monoid a) =>
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

runTest :: (Enumerable a,Show a,Ord a,Arbitrary a,Monoid a) => a -> Int -> IO ((Int,Int),Bool)
runTest a size = do
    cancelled <- newIORef (0 :: Int)
    tests <- newIORef (0 :: Int)
    res <- quickCheckWithResult stdArgs { maxSuccess = size, maxSize = size }
        (prop_equal a (modifyIORef tests succ) (modifyIORef cancelled succ) 10 10000)
    ts <- readIORef tests
    cs <- readIORef cancelled
    return $ ((ts,cs),isSuccess res)


main :: IO ()
main = do
    (times,tests) <- unzip <$> sequence
      -- [ runTest (undefined :: Desc Int) 20
         [ runTest (undefined :: Int) 200
        ]
    forM_ times $ \(ts,cs) -> putStrLn $ show ts ++ " tests, " ++ show cs ++ " cancelled."
    unless (and tests) exitFailure


{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Promise
import Control.Concurrent.STM.Promise.Tree
import Control.Concurrent.STM.DTVar
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.IORef
import Data.Semigroup
import Test.Feat
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

eval :: (Semigroup a,Ord a) => Tree a -> [a]
eval (Leaf x)            = return x
eval (Node Both t1 t2)   = (<>) <$> eval t1 <*> eval t2
eval (Node Either t1 t2) = eval t1 ++ eval t2

deriveEnumerable ''Label
deriveEnumerable ''Tree

instance Enumerable a => Arbitrary (Tree a) where
    arbitrary = sized uniform

instance Semigroup Int where
    (<>) = (+)

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
            t <- choose (0,timeout * 2)
            return $ Leaf <$> delayPromise b t
        Node lbl t1 t2 -> liftM2 (Node lbl) <$> go t1 <*> go t2

prop_equal :: IO () -> IO () -> Int -> Int -> Tree Int -> Property
prop_equal add_test add_cancelled cores timeout tree = do
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

main :: IO ()
main = do
    cancelled <- newIORef 0
    tests <- newIORef 0
    quickCheck (prop_equal (modifyIORef tests succ) (modifyIORef cancelled succ) 10 10000)
    ts <- readIORef tests
    cs <- readIORef cancelled
    putStrLn $ show ts ++ " tests, " ++ show cs ++ " cancelled."


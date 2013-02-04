{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.Promise
import Control.Concurrent.STM.Promise.Tree
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Function
import Data.Monoid
import Test.Feat
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

eval :: Tree Bool -> Bool
eval (Leaf x) = x
eval (And xs) = all eval xs
eval (Or xs)  = any eval xs

deriveEnumerable ''Tree

instance Enumerable a => Arbitrary (Tree a) where
    arbitrary = sized uniform

instance Monoid Bool where
    mempty  = False
    mappend = (&&)

boolPromise :: Bool -> Int -> IO (Promise Bool)
boolPromise b t = do

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

worker :: Maybe Int -> TChan (Promise a) -> IO ()
worker m_t ch = fix $ \ loop -> do
    m_promise <- atomically $ tryReadTChan ch
    case m_promise of
        Nothing -> return ()
        Just promise -> do
            spawn promise

            -- Wait until promise is completed, and cancel it after the timeout
            -- if it is given
            timeout_var <- case m_t of
                Just timeout -> registerDelay timeout
                Nothing -> newTVarIO False

            perform_cancel <- atomically $ do
                status <- result promise
                timed_out <- readTVar timeout_var
                case () of
                    _ | timed_out -> return True
                      | isUnfinished status -> retry
                      | otherwise -> return False
                        -- ^ either cancelled or An a

            when perform_cancel (cancel promise)

            loop

-- Evaluate these promises on n processors, using maybe a timeout in
-- microseconds
workers :: Maybe Int -> Int -> [Promise a] -> IO ()
workers m_t n xs = do
    ch <- newTChanIO
    atomically $ mapM_ (writeTChan ch) xs
    replicateM_ n $ forkIO $ worker m_t ch

mkPromiseTree :: Int -> Tree Bool -> Gen (IO (Tree (Promise Bool)))
mkPromiseTree timeout = go where
    go t = case t of
        Leaf b -> do
            t <- choose (0,timeout-1)
            return $ Leaf <$> boolPromise b t
        And bs -> go' And bs
        Or bs  -> go' Or bs

    go' mk bs = do
        trees <- mapM go bs
        return (mk <$> sequence trees)

prop_equal :: Int -> Int -> Tree Bool -> Property
prop_equal cores timeout tree = monadicIO $
    forAllM (Blind <$> mkPromiseTree timeout tree) $ \ (Blind io_promise_tree) -> run $ do
        putStrLn "== New test =="
        ws <- newTVarIO 0
        promise_tree <- io_promise_tree
        workers (Just timeout) cores (promises promise_tree)
        tree_var <- watchTree ws promise_tree
        forkIO $ fix (\ loop old -> do
            (v,t) <- atomically $ do
                v <- readTVar ws
                if v == old then retry else return v
                (,) v <$> readTVar tree_var
            putStrLn $ show v ++ ": " ++ show t
            loop v) $ 0
        atomically $ do
            x <- readTVar tree_var
            case x of
                Leaf Unfinished -> retry
                Leaf (An b) -> return $ b == eval tree
                Leaf Cancelled -> return False
                _ -> retry

main :: IO ()
main = quickCheck (prop_equal 10 1000000)


### STM Promises

[![Build Status](https://travis-ci.org/danr/stm-promise.png?branch=master)](https://travis-ci.org/danr/stm-promise)

# An example

Running the theorem prover eprover in parallel:

    import Prelude hiding (mapM)
    import Data.Traversable

    import Data.List
    import Data.Maybe

    import Control.Concurrent.STM.Promise
    import Control.Concurrent.STM.Promise.Process
    import Control.Concurrent.STM.Promise.Tree
    import Control.Concurrent.STM.Promise.Workers

    {- | A tree for this file structure:
       ├── mul-commutative
       │   ├── induction_x_0.tptp
       │   ├── induction_x_1.tptp
       │   ├── induction_x_y_0.tptp
       │   ├── induction_x_y_1.tptp
       │   ├── induction_x_y_2.tptp
       │   ├── induction_x_y_3.tptp
       │   ├── induction_y_0.tptp
       │   ├── induction_y_1.tptp
       │   └── no_induction_0.tptp
       └── plus-commutative
           ├── induction_x_0.tptp
           ├── induction_x_1.tptp
           ├── induction_x_y_0.tptp
           ├── induction_x_y_1.tptp
           ├── induction_x_y_2.tptp
           ├── induction_x_y_3.tptp
           ├── induction_y_0.tptp
           ├── induction_y_1.tptp
           └── no_induction_0.tptp
    -}
    file_tree :: Tree FilePath
    file_tree = fmap (++ ".tptp") $ tryAll $
       [ fmap ("mul-commutative/" ++) $ requireAny $
         [ fmap ("induction_x_" ++) $ requireAll $ map Leaf ["0","1"]
         , fmap ("induction_y_" ++) $ requireAll $ map Leaf ["0","1"]
         , fmap ("induction_x_y_" ++) $ requireAll $ map Leaf ["0","1","2","3"]
         , Leaf "no_induction_0"
         ]
       , fmap ("plus-commutative/" ++) $ requireAny $
         [ fmap ("induction_x_" ++) $ requireAll $ map Leaf ["0","1"]
         , fmap ("induction_y_" ++) $ requireAll $ map Leaf ["0","1"]
         , fmap ("induction_x_y_" ++) $ requireAll $ map Leaf ["0","1","2","3"]
         , Leaf "no_induction_0"
         ]
       ]

    success :: ProcessResult -> Bool
    success r = excode r == ExitSuccess && any (`isInfixOf` stdout r) ok
      where
        ok = ["Theorem","Unsatisfiable"]

    eproverPromise :: FilePath -> IO (Promise [(FilePath,Bool)])
    eproverPromise file = do
        let args = ["-xAuto","-tAuto",'-':"-tptp3-format","-s"]
        promise <- processPromise "eprover" (file : args) ""
        let chres :: ProcessResult -> [(FilePath,Bool)]
            chres r = [ (file,success r) ]
        return $ fmap chres promise

    main :: IO ()
    main = do
        promise_tree <- mapM eproverPromise file_tree

        let timeout      = 1000 * 1000 -- microseconds
            processes    = 2

        workers (Just timeout) processes (interleave promise_tree)

        m_res <- evalTree (any (not . snd)) promise_tree

        let res = fromMaybe [] m_res

        putStrLn $ "Results: "

        mapM_ print res

The result of this run is:

    Results:
    ("plus-commutative/induction_x_y_0.tptp",True)
    ("plus-commutative/induction_x_y_1.tptp",True)
    ("plus-commutative/induction_x_y_2.tptp",True)
    ("plus-commutative/induction_x_y_3.tptp",True)

This means that four out of four obligations for commutativity of plus
succeeded when doing induction on both x and y.


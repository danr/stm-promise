
import Prelude hiding (mapM)
import Data.Traversable

import Data.List
import Data.Maybe

import Control.Concurrent.STM.Promise
import Control.Concurrent.STM.Promise.Process
import Control.Concurrent.STM.Promise.Tree
import Control.Concurrent.STM.Promise.Workers

import System.Environment

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
        cmd  = "eprover"
    {-
    let args = ["-tptp","-nw"]
        cmd  = "z3"
        -}
    promise <- processPromise cmd (file : args) ""
    let chres :: ProcessResult -> [(FilePath,Bool)]
        chres r = [ (file,success r) ]
    return $ fmap chres promise

main :: IO ()
main = do
    tm:_ <- map read `fmap` getArgs

    promise_tree <- mapM eproverPromise file_tree

    let timeout      = tm * 1000 -- microseconds
        processes    = 2

    cancel <- workers (Just timeout) processes (interleave promise_tree)

    m_res <- evalTree (any (not . snd)) promise_tree

    let res = fromMaybe [] m_res

    putStrLn $ "Results: "

    mapM_ print res

    cancel



----------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Peter Robinson
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Peter Robinson <pwr@lowerbound.io>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- EXAMPLE USAGE:
--
-- > range=4 expon=6 stack bench +RTS -N8 -RTS  --benchmark-arguments='--output results-32.html --csv results-32.csv'
--
-- The above will run all benchmarks with 10^6 requests split among 32 threads
-- and keys chosen from a range 10^4. You need to change the integer after "-N"
-- according to the number of cores that you have. Saves the results in files
-- results-32.html and results-32.csv.
--
----------------------------------------------------------------------
{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, AllowAmbiguousTypes #-}

module Main where

import Data.Dictionary
import Data.Dictionary.Request

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Random
import Data.IORef
import Control.Concurrent.STM
import Control.Monad.STM
import Control.Monad.ST
import Criterion.Main
import System.Environment
import Data.Atomics
import Prelude hiding (lookup)

import qualified Data.HashTable as H
import qualified Data.IntMap.Strict as M
import qualified Data.HashMap.Strict as UO
import qualified StmContainers.Map as SM

-- Dictionary data type instances for all the test containers:

newtype STMContainersMap a = STMContainersMap (SM.Map Int a)
instance Dictionary (STMContainersMap Int) Int IO where
    runRequest (Lookup k) (STMContainersMap m) = do
        r <- atomically $ SM.lookup k m
        case r of
            Nothing -> return False
            Just _  -> return True
    runRequest (Insert k a) (STMContainersMap m) = atomically $ SM.insert k a m >> SM.null m
    runRequest (Delete k) (STMContainersMap m) = atomically $ SM.delete k m >> SM.null m

newtype MVarHashMap a = MVarHashMap (MVar (UO.HashMap Int a))
instance Dictionary (MVarHashMap Int) Int IO where
    runRequest (Lookup k) (MVarHashMap m) = do
        mymap <- readMVar m
        case UO.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) (MVarHashMap m) =
        modifyMVar m $ \mymap -> do
            let mymap' = UO.insert k a mymap
            return (mymap',UO.null mymap')
    runRequest (Delete k) (MVarHashMap m) =
        modifyMVar m $ \mymap -> do
            let mymap' = UO.delete k mymap
            return (mymap',UO.null mymap')

newtype IORefPrimOps a = IORefPrimOps (IORef (UO.HashMap Int a))
instance Dictionary (IORefPrimOps Int) Int IO where
    runRequest (Lookup k) (IORefPrimOps m) = do
        mymap <- readIORef m
        case UO.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) (IORefPrimOps m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = UO.insert k a mymap in
            (mymap',UO.null mymap')
    runRequest (Delete k) (IORefPrimOps m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = UO.delete k mymap in
            (mymap',UO.null mymap')

newtype IORefPrimOpsHashMap a = IORefPrimOpsHashMap (IORef (UO.HashMap Int a))
instance Dictionary (IORefPrimOpsHashMap Int) Int IO where
    runRequest (Lookup k) (IORefPrimOpsHashMap m) = do
        mymap <- readIORef m
        case UO.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) (IORefPrimOpsHashMap m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = UO.insert k a mymap in
            (mymap',UO.null mymap')
    runRequest (Delete k) (IORefPrimOpsHashMap m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = UO.delete k mymap in
            (mymap',UO.null mymap')

newtype IORefHashMap a = IORefHashMap (IORef (UO.HashMap Int a))
instance Dictionary (IORefHashMap Int) Int IO where
    runRequest (Lookup k) (IORefHashMap m) = do
        mymap <- readIORef m
        case UO.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) (IORefHashMap m) =
        atomicModifyIORef' m $ \mymap ->
            let !mymap' = UO.insert k a mymap in
            (mymap',UO.null mymap')
    runRequest (Delete k) (IORefHashMap m) =
        atomicModifyIORef' m $ \mymap ->
            let !mymap' = UO.delete k mymap in
            (mymap',UO.null mymap')

newtype TVarHashMap a = TVarHashMap (TVar (UO.HashMap Int a))
instance Dictionary (TVarHashMap Int) Int IO where
    runRequest (Lookup k) (TVarHashMap m) = do
        mymap <- readTVarIO m
        case UO.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) (TVarHashMap m) =
        atomically $ stateTVar m $ \mymap ->
            let !mymap' = UO.insert k a mymap in
            (UO.null mymap',mymap')
    runRequest (Delete k) (TVarHashMap m) =
        atomically $ stateTVar m $ \mymap ->
            let !mymap' = UO.delete k mymap in
            (UO.null mymap',mymap')

newtype TestHashTable a = TestHashTable (H.HashTable Int a)
instance Dictionary (TestHashTable Int) Int IO where
    runRequest (Lookup k) (TestHashTable s) = do
        r <- H.lookup s k
        case r of
            Nothing -> return False
            Just _  -> return True
    runRequest (Insert k a) (TestHashTable s) = H.insert s k a
    runRequest (Delete k) (TestHashTable s) = H.delete s k


main = do
    -- use environment variables until we figure out how to avoid interfering with criterions cmdargs handling...
    mThreads <- lookupEnv "threads"
    mRange <- lookupEnv "range"
    mExpon <- lookupEnv "expon"
    mThreshold <- lookupEnv "threshold"
    mResizers <- lookupEnv "numresizers"
    let expon = case mExpon of Just e -> read e
                               Nothing -> 5 -- ^ total number of requests
    let numRequests = 10^expon
    let range = (1,10^(case mRange of Just r -> read r
                                      Nothing -> expon `div` 2))  -- ^ range for keys
    let numThreads = case mThreads of Just t -> read t
                                      Nothing -> 4      -- ^ the number of requests lists we split the list into
    let threshold = case mThreshold of Just t -> read t
                                       Nothing -> 0.75 -- ^ threshold on load for resizing
    let numResizers = case mResizers of Just t -> read t
                                        Nothing -> 8 -- ^ number of worker threads for resizing
    print "Parameters: "
    putStrLn $ "number of spawned threads: " ++ show numThreads
    putStrLn $ "number of requests: " ++ show numRequests
    putStrLn $ "key range: " ++ show range
    putStrLn $ "load threshold: " ++ show threshold
    runBench numThreads numRequests range threshold

runBench :: Int -> Int -> (Int,Int) -> Float -> IO ()
runBench numThreads numRequests range threshold = do
    let genTests = generateTests (numRequests `div` numThreads)
    let genTestsForThreads i = generateTests (numRequests `div` i)

    let mkCHT :: IO (TestHashTable Int)
        mkCHT = TestHashTable <$> H.newWithDefaults 10

    let mkIOPrimOpsMap :: IO (IORefPrimOps Int)
        mkIOPrimOpsMap =
            IORefPrimOps <$> newIORef (UO.empty :: UO.HashMap Int Int)

    let mkIOPrimOpsHashMap :: IO (IORefPrimOpsHashMap Int)
        mkIOPrimOpsHashMap =
            IORefPrimOpsHashMap <$> newIORef (UO.empty :: UO.HashMap Int Int)

    let mkIORefMap :: IO (IORefHashMap Int)
        mkIORefMap =
            IORefHashMap <$> newIORef (UO.empty :: UO.HashMap Int Int)

    let mkTVarMap :: IO (TVarHashMap Int)
        mkTVarMap =
            TVarHashMap <$> newTVarIO (UO.empty :: UO.HashMap Int Int)

    let mkMVarMap :: IO (MVarHashMap Int)
        mkMVarMap =
            MVarHashMap <$> newMVar (UO.empty :: UO.HashMap Int Int)

    let mkSTMMap :: IO (STMContainersMap Int)
        mkSTMMap = STMContainersMap <$> SM.newIO

    numProcs <- getNumCapabilities
    print ("number of available cores: ",numProcs)
    threadDelay 3000000

    let evaluate tests ds = do
            res <- forConcurrently tests
                            (mapM $ \r -> runRequest r ds)
            print $ length $ filter (==True) $ concat res
            return $! filter (==True) $ concat res


   -- Generate set of tests for different workloads
    !tests_90_5_5 <- replicateM numThreads $ genTests (0.9,0.05) range
    !tests_80_10_10 <- replicateM numThreads $ genTests (0.8,0.1) range
    !tests_70_15_15 <- replicateM numThreads $ genTests (0.7,0.15) range
    !tests_60_20_20 <- replicateM numThreads $ genTests (0.6,0.2) range
    !tests_50_25_25 <- replicateM numThreads $ genTests (0.5,0.25) range
    !tests_40_30_30 <- replicateM numThreads $ genTests (0.4,0.30) range
    -- !tests_30_35_35 <- replicateM numThreads $ genTests (0.3,0.35) range
    !tests_20_40_40 <- replicateM numThreads $ genTests (0.2,0.40) range
    -- !tests_10_45_45 <- replicateM numThreads $ genTests (0.1,0.45) range
    !tests_0_50_50 <- replicateM numThreads $ genTests (0,0.5) range
    -- let !all_95_25_25 = concat tests_95_25_25
    let !all_90_5_5 = concat tests_90_5_5
    let !all_80_10_10 = concat tests_80_10_10
    let !all_70_15_15 = concat tests_70_15_15
    let !all_60_20_20 = concat tests_60_20_20
    let !all_50_25_25 = concat tests_50_25_25
    let !all_40_30_30 = concat tests_40_30_30
    -- let !all_30_35_35 = concat tests_30_35_35
    let !all_20_40_40 = concat tests_20_40_40
    -- let !all_10_45_45 = concat tests_10_45_45
    let !all_0_50_50 = concat tests_0_50_50
    print "--------------------------------------------"
    print "90 5 5:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_90_5_5)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_90_5_5)
    print $ "# of delete requests: "++ show (length $ filter isDelete  all_90_5_5)
    print "--------------------------------------------"
    print "80 10 10:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_80_10_10)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_80_10_10)
    print $ "# of delete requests: "++ show (length $ filter isDelete all_80_10_10)
    print "--------------------------------------------"
    print "70 15 15:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_70_15_15)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_70_15_15)
    print $ "# of delete requests: "++ show (length $ filter isDelete  all_70_15_15)
    print "--------------------------------------------"
    print "60 20 20:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_60_20_20)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_60_20_20)
    print $ "# of delete requests: "++ show (length $ filter isDelete  all_60_20_20)
    print "--------------------------------------------"
    print "50 25 25:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_50_25_25)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_50_25_25)
    print $ "# of delete requests: "++ show (length $ filter isDelete  all_50_25_25)
    print "--------------------------------------------"
    print "40 30 30:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_40_30_30)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_40_30_30)
    print $ "# of delete requests: "++ show (length $ filter isDelete  all_40_30_30)
    print "--------------------------------------------"
    print "20 40 40:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_20_40_40)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_20_40_40)
    print $ "# of delete requests: "++ show (length $ filter isDelete  all_20_40_40)
    print "--------------------------------------------"
    print "0 50 50:"
    print $ "# of lookup requests: "++ show (length $ filter isLookup  all_0_50_50)
    print $ "# of insert requests: "++ show (length $ filter isInsert  all_0_50_50)
    print $ "# of delete requests: "++ show (length $ filter isDelete  all_0_50_50)
    print "--------------------------------------------"
    defaultMain
    -- run benchmark for different workloads:
         [ bgroup "0% Lookups; 50% Inserts; 50% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_0_50_50)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_0_50_50)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_0_50_50)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_0_50_50)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_0_50_50)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_0_50_50)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_0_50_50)
            ]
        , bgroup "20% Lookups; 40% Inserts; 40% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_20_40_40)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_20_40_40)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_20_40_40)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_20_40_40)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_20_40_40)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_20_40_40)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_20_40_40)
            ]
        , bgroup "40% Lookups; 30% Inserts; 30% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_40_30_30)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_40_30_30)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_40_30_30)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_40_30_30)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_40_30_30)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_40_30_30)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_40_30_30)
            ]
        , bgroup "50% Lookups; 25% Inserts; 25% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_50_25_25)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_50_25_25)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_50_25_25)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_50_25_25)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_50_25_25)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_50_25_25)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_50_25_25)
            ]
        ,  bgroup "60% Lookups; 20% Inserts; 20% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_60_20_20)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_60_20_20)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_60_20_20)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_60_20_20)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_60_20_20)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_60_20_20)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_60_20_20)
            ]
         , bgroup "70% Lookups; 15% Inserts; 15% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_70_15_15)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_70_15_15)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_70_15_15)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_70_15_15)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_70_15_15)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_70_15_15)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_70_15_15)
            ]
        , bgroup "80% Lookups; 10% Inserts; 10% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_80_10_10)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_80_10_10)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_80_10_10)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_80_10_10)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_80_10_10)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_80_10_10)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_80_10_10)
            ]
        , bgroup "90% Lookups; 5% Inserts; 5% Deletes"
            [
            bench "IORef atomicModifyIORef HashMap" $ nfIO (mkIORefMap >>= evaluate tests_90_5_5)
            , bench "IORef atomic-primops IntMap" $ nfIO (mkIOPrimOpsMap >>= evaluate tests_90_5_5)
            , bench "IORef atomic-primops HashMap" $ nfIO (mkIOPrimOpsHashMap >>= evaluate tests_90_5_5)
            , bench "TVar HashMap" $ nfIO (mkTVarMap >>= evaluate tests_90_5_5)
            , bench "MVar HashMap" $ nfIO (mkMVarMap >>= evaluate tests_90_5_5)
            , bench "StmContainers.Map" $ nfIO (mkSTMMap >>= evaluate tests_90_5_5)
            , bench "Concurrent HashTable" $ nfIO (mkCHT >>= evaluate tests_90_5_5)
            ]
        ]

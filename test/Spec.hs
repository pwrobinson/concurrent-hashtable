{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Map.Strict as M
import Test.QuickCheck         as QC
import Test.QuickCheck.Monadic as QC
import Prelude hiding (lookup)
import qualified Data.Set as Set

import qualified Data.HashTable as H
import Control.Concurrent.STM
import Data.Hashable
import Data.Dictionary
import Data.Dictionary.Request

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

-- | Restrict the keys to be ints from small positive range
newtype BoundedInt = BoundedInt Int
    deriving (Eq,Ord)

instance Show BoundedInt where
    show (BoundedInt i) = show i

instance Hashable BoundedInt where
    hashWithSalt a (BoundedInt i) = hashWithSalt a i

instance QC.Arbitrary BoundedInt where
    arbitrary = BoundedInt <$> QC.choose (1,100)

type TestMap = M.Map BoundedInt BoundedInt
instance Dictionary (IORef TestMap) BoundedInt IO where
    runRequest (Lookup k) m = do
        mymap <- readIORef m
        case M.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) m =
        atomicModifyIORef' m $ \mymap ->
                let s1 = M.size mymap in
                let mymap' = M.insert k a mymap in
                let s2 = M.size mymap' in
                (mymap',s1/=s2)
    runRequest (Update k a) m =
        atomicModifyIORef' m $ \mymap ->
            case M.lookup k mymap of
                Nothing -> (mymap,False)
                Just _ ->
                    let s1 = M.size mymap in
                    let mymap' = M.insert k a mymap in
                    let s2 = M.size mymap' in
                    (mymap',s1/=s2)
    runRequest (Delete k) m =
        atomicModifyIORef' m $ \mymap ->
            let s1 = M.size mymap in
            let mymap' = M.delete k mymap in
            let s2 = M.size mymap' in
            (mymap',s1/=s2)


type TestChainHashTable = H.HashTable BoundedInt BoundedInt
instance Dictionary TestChainHashTable BoundedInt IO where
    runRequest (Lookup k) s = do
        r <- H.lookup s k
        case r of
            Nothing -> return False
            Just _  -> return True
    runRequest (Insert k a) s = H.insert s k a
    runRequest (Update k a) s = H.insert s k a
    runRequest (Delete k) s = H.delete s k


prop :: IORef TestMap -> TestChainHashTable -> Property
prop ioref chainTable = monadicIO $ do
    (r :: Request BoundedInt) <- pick arbitrary
    run $ appendFile "request_sequence.txt" (show r ++ "\n")
    run $ void (runRequest r ioref >> runRequest r chainTable)

    mapAssocs <- run ( M.assocs <$> readIORef ioref)
    -- test if all entries in the Map are also in the Hash Table:
    resChain <- run $ sequence
                    [ do res <-  H.lookup chainTable k
                         return $ (isJust res) && (fromJust res == a)
                    | (k,a) <- mapAssocs ]
    list2 <- run $ atomically $ H.readAssocs chainTable
    -- test if there aren't any duplicates in the Hash Table:
    let res2 = (not . hasDuplicates) list2
    mymap <- run $ readIORef ioref
    -- test if all entries in the Hash Table are also in the Map:
    let res3 = [ isJust $ M.lookup (fst k) mymap | k <- list2 ]
    -- run $ print =<< atomically (H.readAssocs chainTable)
    assert $ and resChain && res2 && and res3


main :: IO ()
main = do
    ioref <- newIORef (M.empty :: M.Map BoundedInt BoundedInt)
    (ctable :: H.HashTable BoundedInt BoundedInt) <- H.newWithDefaults 2
    writeFile "request_sequence.txt" ""
    quickCheckWith stdArgs{ maxSuccess = 50000 } $ prop ioref ctable
    print "------- Map ASSOCS List ----"
    print =<< (M.assocs <$> readIORef ioref)
    print "--------Hashtable AFTER --------"
    print =<< atomically (H.readAssocs ctable)

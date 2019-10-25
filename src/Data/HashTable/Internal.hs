----------------------------------------------------------------------
-- |
-- Module      :  Data.HashTable.Internal
-- Copyright   :  (c) Peter Robinson
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Peter Robinson <pwr@lowerbound.io>
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency, stm)
--
-- You can find benchmarks and more information about the internals of this package here:  <https://lowerbound.io/blog/2019-10-24_concurrent_hash_table_performance.html>
--
-- List of atomic operations:
-- /insert/, /insertIfNotExists/, /lookup/, /delete/, /getAssocs/, /resize/
----------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module Data.HashTable.Internal
where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef
import Data.Atomics
import Control.Exception
import Control.Monad
import Data.Hashable
import System.Random
import Data.Maybe
import qualified Data.List as L
import Data.Vector(Vector,(!))
import qualified Data.Vector as V
import Prelude hiding (lookup,readList)

-- Used internally.
data MigrationStatus = NotStarted | Ongoing | Finished
        deriving(Show,Eq)

-- | Used for chain-hashing.
data Chain k v = Chain
    { _itemsTV           :: TVar [(k,v)]   -- ^ stores the
    , _migrationStatusTV :: TVar MigrationStatus -- ^ used internally for resizing
    }
    deriving (Eq)

newChainIO :: IO (Chain k v)
newChainIO =
    Chain <$> newTVarIO []
          <*> newTVarIO NotStarted

-- | A thread-safe hash table that supports dynamic resizing.
data HashTable k v = HashTable
    { _chainsVecTV       :: TVar (Vector (Chain k v)) -- ^ vector of linked lists
    , _totalLoad        :: IORef Int
    , _config           :: Config k
    }

-- | Configuration options that may affect the performance of the hash table
data Config k = Config
    { _scaleFactor      :: Float -- ^ scale factor for resizing
    , _threshold        :: Float -- ^ load threshold for initiating resizing.
    , _numResizeWorkers :: Int   -- ^ maximum number of worker threads used during resizing
    , _hashFunc         :: k -> Int
    }

instance Show (Config k) where
    show cfg = "Config " ++ show (_scaleFactor cfg)
                         ++ show (_threshold cfg)
                         ++ show (_numResizeWorkers cfg)

-- | Default configuration: scale factor = 2.0; resizing threshold = 0.75;
--   number of worker threads for resizing = 'getNumCapabilities';
--   hash function = use 'hashWithSalt' with a random salt
mkDefaultConfig :: Hashable k => IO (Config k)
mkDefaultConfig = do
    numCPUs <- getNumCapabilities
    salt <- randomIO :: IO Int
    return $ Config
        { _scaleFactor = 2.0
        , _threshold = 0.75
        , _numResizeWorkers = numCPUs
        , _hashFunc = hashWithSalt salt
        }


-- | Creates a new hash table with an initial size. See 'newWithDefaults' for more details.
--   You probably either want to use 'newWithDefaults' instead or
--   something like this:
-- > mkDefaultConfig { _field = myValue } >>= new 10
new :: (Eq k) => Int  -- ^ Initial size of the hash table
              -> Config k -> IO (HashTable k v)
new size config = do
    chainsVec <- V.replicateM size newChainIO
    HashTable <$> newTVarIO chainsVec
              <*> newIORef 0
              <*> return config

-- | Creates a new hash table with the given initial vector size, scale factor 2.0, a resizing load threshold of 0.75, and we use as many threads for resizing as we have cores available. This  will use a hash function with a (single) random salt, so if you need security, you MUST supply your own hash function. To be replaced by universal hashing in future versions.
newWithDefaults :: (Eq k,Hashable k) => Int -- ^ Initial size of the hash table
                -> IO (HashTable k v)
newWithDefaults size = mkDefaultConfig >>= new size

-- | Returns the size of the vector representing the hash table.
{-# INLINABLE readSizeIO #-}
readSizeIO :: HashTable k v -> IO Int
readSizeIO ht = do
    chainsVec <- readTVarIO $ _chainsVecTV ht
    return $ V.length chainsVec


-- | Resizes the hash table by scaling it according to the _scaleFactor in the configuration.
resize :: (Eq k)
       => HashTable k v -> IO ()
resize ht = do
    chainsVec <- readTVarIO $ _chainsVecTV ht
    let size1 = V.length chainsVec
    alreadyResizing <- do
        hasStarted <- atomically $ do
            migrating <- readTVar (_migrationStatusTV $ chainsVec ! 0)
            if migrating `elem` [Ongoing,Finished] then
                return True
            else do
                writeTVar (_migrationStatusTV $ chainsVec ! 0) Ongoing
                return False
        size2 <- readSizeIO ht
        return (hasStarted || (size1 /= size2))
    unless alreadyResizing $ do
        let oldSize = V.length chainsVec
        let numWorkers = _numResizeWorkers $ _config ht
        let numSlices = min numWorkers (oldSize `div` numWorkers)
        let sliceLength = oldSize `div` numSlices
        let restLength = oldSize - ((numSlices-1)*sliceLength)
        let vecSlices = [ V.unsafeSlice
                            (i*sliceLength)
                            (if i==numSlices-1 then restLength
                                               else sliceLength)
                            chainsVec
                           | i <- [0..numSlices-1]]
        let (scale :: Float) = _scaleFactor (_config ht)
        let (newSize::Int) = round $ (fromIntegral oldSize) * scale
        newVec <- V.replicateM newSize newChainIO
        forConcurrently_ vecSlices (V.mapM_ (migrate newVec newSize))
                `catch` (\(e::AssertionFailed) -> do
                            debug "ERROR in resize; this should never happen..."
                            throw e)
        debug "finished copying over nodes..."
        -- replace old vector with new one:
        atomically $ writeTVar (_chainsVecTV ht) newVec
        debug "replaced old array with new one..."
        -- now unblock threads that are blocking on the "migrate" field of any of the old chains:
        forConcurrently_ vecSlices $
            V.mapM_ (\chain ->
                        atomically $ writeTVar (_migrationStatusTV chain) Finished)
        debug "woke up blocked threads..."
    where
        migrate newVec newSize chain = do
            -- debug ("starting to copy nodes of list # ",idx)
            atomically $ writeTVar (_migrationStatusTV chain) Ongoing
            -- debug ("updated list status # ",idx)
            listOfNodes <- readTVarIO (_itemsTV chain)
            -- debug ("done copying nodes of list # ",idx)
            sequence_ [ do let newIndex = (_hashFunc (_config ht) k) `mod` newSize
                           let newChain = newVec ! newIndex
                           newList <- readTVarIO (_itemsTV newChain)
                           atomically $
                               writeTVar (_itemsTV newChain) ((k,v):newList)
                      | (k,v) <- listOfNodes ]
            -- debug ("finished migrate for list ",idx)

-- | Lookup the value for the key in the hash table if it exists.
{-# INLINABLE lookup #-}
lookup :: (Eq k) => HashTable k v -> k -> IO (Maybe v)
lookup htable k = do
    chain <- readChainForKeyIO htable k
    list  <- readTVarIO (_itemsTV chain)
    return $ L.lookup k list


-- | Used internally. An action to be executed atomically for the given chain.
type STMAction k v a = TVar [(k,v)] -> STM (Maybe a)

-- | Used internally by 'insert', 'insertIfNotExists', 'delete', 'update'
genericModify :: (Eq k)
       => HashTable k v
       -> k -- ^ key
       -> STMAction k v a
       -> IO a
genericModify htable k stmAction = do
    chain <- readChainForKeyIO htable k
    result <- atomically $ do
        migrationStatus <- readTVar (_migrationStatusTV chain)
        case migrationStatus of
            Ongoing    -> retry          -- block until resizing is done
            Finished   -> return Nothing -- resizing already done; try again
            NotStarted ->                -- no resizing happening at the moment
                stmAction (_itemsTV chain)
    case result of
        Nothing -> genericModify htable k stmAction
        Just v  -> return v

-- | Inserts the key-value pair `k` `v` into the hash table. Uses chain hashing to resolve collisions.
insert :: (Eq k)
       => HashTable k v
       -> k -- ^ key
       -> v -- ^ value
       -> IO Bool
insert htable k v = do
    result <- genericModify htable k $ \tvar -> do
                list <- readTVar tvar
                case L.lookup k list of
                    Nothing -> do
                        writeTVar tvar ((k,v):list)
                        return $ Just True
                    Just _  -> do -- entry was already there, so we overwrite it
                        writeTVar tvar ((k,v) : deleteFirstKey k list)
                        return $ Just False
    when result $
        atomicallyChangeLoad htable 1
    return result


-- | Inserts a key and value pair into the hash table only if the key does not exist yet. Returns 'True' if the insertion was successful, i.e., the hash table did not contain this key before. To get the same behaviour as 'Data.Map.insert', use 'insert'. If you want to update an entry only if it exists, use 'update'.
insertIfNotExists :: (Eq k)
       => HashTable k v
       -> k -- ^ key
       -> v -- ^ value
       -> IO Bool
insertIfNotExists htable k v = do
    result <- genericModify htable k $ \tvar -> do
                list <- readTVar tvar
                case L.lookup k list of
                    Nothing -> do
                        writeTVar tvar ((k,v):list)
                        return $ Just True
                    Just _  -> return $ Just False
    when result $
        atomicallyChangeLoad htable 1
    return result

-- | Deletes the entry for the given key from the hash table. Returns `True` if and only if an entry was deleted from the table.
delete :: (Eq k)
       => HashTable k v
       -> k -- ^ key of entry that will be deleted
       -> IO Bool
delete htable k = do
    result <- genericModify htable k $ \tvar -> do
                list <- readTVar tvar
                case L.lookup k list of
                    Nothing ->
                        return $ Just False
                    Just _  -> do
                        writeTVar tvar $ deleteFirstKey k list
                        return $ Just True
    when result $
        atomicallyChangeLoad htable (-1)
    return result

-- | Atomically increment/decrement the table load value by adding the provided integer value to the current value.
atomicallyChangeLoad :: (Eq k)
                     => HashTable k v
                     -> Int -- ^ increment/decrement value
                     -> IO ()
atomicallyChangeLoad htable incr = do
    totalLoad <- atomicModifyIORefCAS (_totalLoad htable) $
                    \l -> (l+incr,l+incr)
    size <- readSizeIO htable
    when ((fromIntegral totalLoad / fromIntegral size)
                        >= _threshold (_config htable)) $ do
        chain0 <- readChainForIndexIO htable 0
        migrationStatus <- readTVarIO (_migrationStatusTV chain0)
        when (migrationStatus == NotStarted) $
            void $ forkIO (resize htable)

-- Atomically retrieves list of key-value pairs. If there is a lot of contention going on, this may be very inefficient.
getAssocs :: (Eq k)
            => HashTable k v -> STM [(k,v)]
getAssocs htable = do
    chainsVec <- readTVar $ _chainsVecTV htable
    let len = V.length chainsVec
    let getItemsForChain k = do
            chain <- readChainForIndex htable k
            readTVar (_itemsTV chain)
    msum <$> mapM getItemsForChain [0..len-1]

{-# INLINABLE deleteFirstKey #-}
deleteFirstKey ::  Eq a => a -> [(a,b)] -> [(a,b)]
deleteFirstKey _ []     = []
deleteFirstKey x (y:ys) = if x == fst y then ys else y : deleteFirstKey x ys

-- | Atomically read the chain for the given key.
{-# INLINABLE readChainForKeyIO #-}
readChainForKeyIO :: HashTable k v -> k -> IO (Chain k v)
readChainForKeyIO htable k = do
    chainsVec <- readTVarIO $ _chainsVecTV htable
    let size = V.length chainsVec
    let index = (_hashFunc (_config htable) k) `mod` size
    return $ chainsVec ! index

-- | Atomically read the chain for the given index. (Warning: bounds are not checked.)
{-# INLINABLE readChainForIndexIO #-}
readChainForIndexIO :: HashTable k v -> Int -> IO (Chain k v)
readChainForIndexIO htable idx = do
    chainsVec <- readTVarIO $ _chainsVecTV htable
    return $ chainsVec ! idx

-- | Atomically read the chain for the given index. (Warning: bounds are not checked.)
{-# INLINABLE readChainForIndex #-}
readChainForIndex :: HashTable k v -> Int -> STM (Chain k v)
readChainForIndex htable idx = do
    chainsVec <- readTVar $ _chainsVecTV htable
    return $ chainsVec ! idx

{-# INLINABLE debug #-}
debug :: Show a => a -> IO ()
debug _ = return () {- do
    -- tid <- myThreadId
    -- print a
    -- appendFile ("thread" ++ show tid) (show a) -}

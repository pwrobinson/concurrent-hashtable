----------------------------------------------------------------------
-- |
-- Module      :  Data.HashTable
-- Copyright   :  (c) Peter Robinson
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Peter Robinson <pwr@lowerbound.io>
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency, stm)
--
-- You can find benchmarks and more information about the internals of this package here:  <https://lowerbound.io/blog/2019-10-24_concurrent_hash_table_performance.html>
--
--
-- Usage Example:
--
-- >> ht <- newWithDefaults 4     -- creates hash table of initial size 4
-- >> insert ht 1 "hello"         -- adds key-value pair (1,"hello")
-- >> insert ht 2 "world"         -- adds key-value pair (2,"world")
-- >> atomically $ readAssocs ht  -- convert to a key-value list
-- > [(1,"hello"),(2,"world")]
-- >> readSizeIO ht               -- returns 4
-- >> insert ht 3 "!"             -- adds key-value pair (3,"!") and triggers a resize since load/size is ≥ 0.75
-- >> readSizeIO ht               -- returns 8
-- >> atomically $ readAssocs ht  -- convert to a key-value list
-- > [(1,"hello"),(3,"!"),(2,"world")]
--
--
----------------------------------------------------------------------

module Data.HashTable(
        -- * Data Type
        HashTable,
        Chain,
        -- * Construction
        new,
        newWithDefaults,
        mkDefaultConfig,
        Config(..),
        -- * Atomic Read-Operations
        lookup,
        readAssocs,
        -- * Non-Atomic Read-Operations
        readAssocsIO,
        -- * Atomic Write-Operations
        insert,
        add,
        update,
        modify,
        delete,
        swapValues,
        -- * Utilities
        readSizeIO,
        readSize,
        readLoad,
        resize
    )
where
import Data.HashTable.Internal
import Prelude hiding (lookup)

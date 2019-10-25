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
----------------------------------------------------------------------

module Data.HashTable(
        HashTable,
        Chain,
        _itemsTV,
        new,
        newWithDefaults,
        mkDefaultConfig,
        Config(..),
        lookup,
        insert,
        insertIfNotExists,
        delete,
        getAssocs
    )
where
import Data.HashTable.Internal
import Prelude hiding (lookup)

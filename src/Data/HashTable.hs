----------------------------------------------------------------------
-- |
-- Module      :  Data.HashTable
-- Copyright   :  (c) Peter Robinson
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Peter Robinson <pwr@lowerbound.io>
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency, stm)
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

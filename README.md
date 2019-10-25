# A thread-safe hash table for multi-cores

You can find benchmarks and more information about the internals of this package [here](https://lowerbound.io/blog/2019-10-24_concurrent_hash_table_performance.html).

## Installation

> stack install

## Usage Example

>> ht <- newWithDefaults 2     -- creates hash table of initial size 4
>> insert ht 1 "hello"         -- adds key-value pair (1,"hello")
>> insert ht 2 "world"         -- adds key-value pair (2,"world")
>> atomically $ readAssocs ht  -- convert to a key-value list
> [(1,"hello"),(2,"world")]
>> readSizeIO ht               -- returns 4
>> insert ht 3 "!"             -- adds key-value pair (3,"!") and triggers a resize as the load fraction is ≥ 0.75
>> readSizeIO ht               -- returns 8
>> atomically $ readAssocs ht  -- convert to a key-value list
> [(1,"hello"),(3,"!"),(2,"world")]

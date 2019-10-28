# Changelog for concurrent-hashtable

## Unreleased changes

## 0.1.8

* Changed `insertIfExists` to `add`.
* Added `swapValues`.
* Added `readAssocsIO`.
* Simplified `STMAction`.
* Improved documentation.
* Fixed bug in `resize` when initial table size is less than `_numResizeWorkers`.

## 0.1.7

* Added `modify`.
* Included `StmContainers.Map` in the benchmarks.
* Haddock updates.

Gorillas Collections
====================

Fast, lightweight collections aimed to be used with sorted data.

Goals
-----

1. Faster than current Scala collections implementations
2. Significantly less memory footprint than current Scala implementations
3. Easy to manipulate for quick computations
4. No dependencies on other libraries except Scala's and Java's standard API

The code has been optimized by looking at the javap output after compilation and by running mico-benchmarks.  Don't expect a pure functional implementation as a result.

Collections
-----------

### NavigableMap ###

A sorted map with "floorKey", "ceilingKey", "lowerKey", and "higherKey" method implementations.  It also provides fast iteration over its sorted keys.

Example:

import gorillas.collection.immutable.NavigableMap

val sampleMap = NavigableMap((7 -> "Seven"), (3 -> "Three"), (5 -> "Five"))

Performance:
Creating the collection: O(N) *  O(Log(N))
Retrieval: best case: O(1); average: is O(Lg(N)); worst: O(N)
Memory: O(N)

### NavigableMultiMap ###

NavigableMultiMap

A sorted map that provides association of multiple values under the same key.  The keys are ordered.  The values for the same key are sorted according to their insertion order.

This is still work in progress.

Copyright
---------

Copyright &copy; 2012, Ricardo Le&oacute;n

All rights reserved.

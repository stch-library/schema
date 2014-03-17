# History

## 0.3.1

1. Moved utility functions to separate project.
2. Added docs.

## 0.3.0

1. Replaced eq, pair, one, optional, record, protocol, pred, and enum with Eq, Pair, One, Optional, Record, Protocol, Predicate, and Enumerate, respectively.
2. Changed One record to Single.
3. Updated speclj dependency to latest.
4. Added always-validate test.
5. Changed project name and added to Clojars.

## 0.2.1

1. Replaced loop with let in process-fn-arity to address https://github.com/Prismatic/schema/issues/65.
2. Simplistic attempt to add comma separation to the arglist.
3. Added test for loop/recur.
4. recur without an explicit call to loop is NOT supported by this library.  You must use recur within an explicit call to loop.  See tests for examples.

## 0.2.0

1. Changed defn*, defrecord*, fn*, letfn* to defn', defrecord', fn', and letfn'.
2. Added tests for fn' and letfn'.

## 0.1.0

1. Initial release.

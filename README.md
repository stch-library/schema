# stch.schema

Forked from https://github.com/Prismatic/schema

## Motivations for forking:

1. Ability to "use" rather than "require".
2. Bring certain features more in line with core.typed.
3. Focus on Clojure first and foremost.
4. I have stubbornness issues.

## Differences:

1. defn, defrecord, fn, and letfn are now defn', defrecord', fn', and leftn', repectively.
2. pred is now a macro (called Predicate), so you can do (Predicate even?) instead of (pred even? 'even?).
3. => and =>* are now just Fn, and have a single syntax.  For example, (validate (Fn Int [Int]) inc).  You can also do (Fn) which produces (Fn Any [& [Any]]) for when you really don't want to think about the types (they're not checked anyways).  FnSchema uses ifn? instead of fn?, something that was recently fixed in Prismatic Schema.
4. one, optional, and pair can be unnamed, and have been renamed One, Optional, and Pair, respecitvely.  For example, (One Int) or (Pair Keyword String).  The errors messages reference the element position: elem0, elem1, etc. in this scenario.
5. There are no dependencies.  Macros were moved to core namespace.
6. Lot's of "built-in" types. Int, Num, Keyword, Symbol, Ratio, Atom, Date, UUID, Named, Map, Set, (Vector String), (Vector), (List String), (List), (Queue String), (Queue).
7. maybe is now Option.  For example, (Option String).
8. either is now U, short for union.  For example, (U String Keyword).
9. both is now I, short for intersection.  For example, (I (Predicate vector?) [String]).
10. A few minor bug fixes.  For example, "every?" should be "some" in the error output of either/U.
11. test and coerce namespace are not included.
12. recur without an explicit call to loop is NOT supported by this library.  You must use recur within an explicit call to loop.  See tests for examples.

I have my own set of unit-tests, around 120.  Code coverage is pretty decent, though, not as exhaustive as Prismatic Schema.

Many thanks to Prismatic for sharing this awesome library.  It really is game changing in my opinion.  Please feel free to incorporate any changes into Prismatic Schema.  I will add significant changes from Prismatic Schema into this code base as they occur.  Most notably would be support for:

```clojure
(defn' create-user
  [user-id :- Int & {:keys [name email]} :- {:name String :email String}])
```

This code will be used in production, but currently is not.

Feedback is welcome.  New features should be directed to Prismatic Schema.

## Installation

Add the following to your project dependencies:

```clojure
[stch-library/schema "0.3.3"]
```

## API Documentation

http://stch-library.github.io/schema

## How to use

```clojure
(use 'stch.schema)
```

Functions
```clojure
(defn' rest-args :- (Vector String)
  [name :- String, age :- Int & likes :- [String]]
  (vec likes))

(defn' multi-arity :- String
  ([url :- String] (multi-arity url true))
  ([url :- String
    follow-redirects? :- Boolean]
   url))

(defn' higher-order :- (Fn Int [Int])
  [f :- (Fn Int [Int])]
  f)

(defn' any-fn :- (Fn)
  [f :- (Fn)]
  f)

(validate (Fn Int [Int]) inc)
(validate (Fn String [Map]) :name)
```

Records
```clojure
(defrecord' Employee
  [hired :- Date
   position :- Keyword
   roles :- [String]])
```

Built-in types
```clojure
(validate String "Billy")
(validate Boolean true)
(validate Symbol 'a)
(validate Date #inst "2014-02-06")
(validate Atom (atom [1 2 3]))

(validate Set #{"Billy" :Bobby})

(validate Map {})
(validate Map {:name "Billy"})

(validate (Vector String) ["hi" "hello"])
(validate (Vector) ["Bob" :Billy])

(validate (List String) '("hi" "hello"))
(validate (List) '("Bob" :Billy))

(validate Named 'Billy)
(validate Named "Billy")
(validate Named :Billy)

(validate (Option String) "Billy")
(validate (Option String) nil)
```

Sequences
```clojure
(validate [(One Int) Double] [1234 95.6])
(validate [(Optional Int) (Optional String)] [])
(validate [(Pair String Int)] [["David" 35]["Billy" 37]])
```

Predicates
```clojure
(def Set (Predicate set?))
```

Union and Intersection
```clojure
(def Named (U String Symbol Keyword))
(defn List [x] (I (Predicate list?) [x]))
```

## Unit-tests

Run "lein spec"












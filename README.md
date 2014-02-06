# stch.schema

## Motivations for forking:

1. Ability to "use" rather than "require".
2. Bring certain features more in line with core.typed.
3. Focus on Clojure first and foremost.
4. I have stubbornness issues.

## Differences:

1. defn, defrecord, fn, and letfn are now defn*, defrecord*, fn*, and leftn*, repectively.
2. pred is now a macro so you can do (pred even?) instead of (pred even? 'even?).
3. => and =>* are now just Fn, and have a single syntax.  For example, (validate (Fn Int [Int]) inc).  You can also do (Fn) which produces (Fn Any [& [Any]]) for when you really don't want to think about the types (they're not checked anyways).  FnSchema uses ifn? instead of fn?, something that was recently fixed in Prismatic Schema.
4. one, optional, and pair can be unnamed.  For example, (one Int) or (pair Keyword String).  The errors messages reference the element position: elem0, elem1, etc. in this scenario.
5. There are no dependencies.  Macros were moved to core namespace.
6. Lot's of "built-in" types. Int, Num, Keyword, Symbol, Ratio, Date, UUID, Named, Map, Set, (Vector String), (Vector), (List String), (List), (Queue String), (Queue).
7. maybe is now Option.  For example, (Option String).
8. either is now U, short for union.  For example, (U String Keyword).
9. both is now I, short for intersection.  For example, (I (pred vector?) [String]).
10. A few minor bug fixes.  For example, "every?" should be "some" in the error output of either/U.

I have my own set of unit-tests, 112 to be exact.  Code coverage is pretty decent, though, not as exhaustive as Prismatic Schema.

Many thanks to Prismatic for sharing this awesome library.  It really is game changing in my opinion.  Please feel free to incorporate any changes into Prismatic Schema.  I will add significant changes from Prismatic Schema into this code base as they occur.   Most notably would be support for:

```Clojure
(defn* create-user
  [user-id :- Int & {:keys [name email]} :- {:name String :email String}])
```

This code will be used in production, but currently is not.

Feedback is welcome.  New features should be directed to Prismatic Schema.

## How to use

1. Clone repo locally
2. cd into cloned repo
3. run "lein install"
4. Add [stch.schema "0.1.0"] to your project dependencies.

## Unit-tests

Run "lein spec"












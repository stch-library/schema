(ns stch.schema
  "A library for data shape definition and validation.
  A Schema is just Clojure data, which can be used to
  document and validate Clojure functions and data.

  For example,

  (def FooBar {:foo Keyword :bar [Number]}) ;; a schema

  (check FooBar {:foo :k :bar [1.0 2.0 3.0]})
  ==> nil

  representing successful validation, but the following
  all return helpful errors describing how the provided
  data fails to measure up to schema FooBar's standards.

  (check FooBar {:bar [1.0 2.0 3.0]})
  ==> {:foo missing-required-key}

  (check FooBar {:foo 1 :bar [1.0 2.0 3.0]})
  ==> {:foo (not (keyword? 1))}

  (check FooBar {:foo :k :bar [1.0 2.0 3.0] :baz 1})
  ==> {:baz disallowed-key}

  Schema lets you describe your leaf values using the
  Any, Keyword, Number, String, and Int definitions below,
  or (in Clojure) you can use arbitrary Java classes or
  primitive casts to describe simple values.

  From there, you can build up schemas for complex types
  using Clojure syntax (map literals for maps, set literals
  for sets, vector literals for sequences, with details
  described below), plus helpers below that provide optional values,
  enumerations, arbitrary predicates, and more.

  Schema also provides macros for defining records with
  schematized elements (defrecord'), and named or anonymous
  functions (fn' and defn') with schematized inputs and
  return values.  In addition to producing better-documented
  records and functions, these macros allow you to retrieve
  the schema associated with the defined record or function.
  Moreover, functions include optional *validation*, which will throw
  an error if the inputs or outputs do not match the provided schemas:

  (defrecord' FooBar
    [foo :- Int
     bar :- String])

  (defn' quux :- Int
    [foobar :- Foobar
     mogrifier :- Number]
    (* mogrifier (+ (:foo foobar) (Long/parseLong (:bar foobar)))))

  (quux (FooBar. 10 \"5\") 2)
  ==> 30

  (fn-schema quux)
  ==> (Fn Int [(record user.FooBar {:foo Int, :bar java.lang.String}) java.lang.Number])

  (with-fn-validation (quux (FooBar. 10.2 \"5\") 2))
  ==> Input to quux does not match schema: [(named {:foo (not (integer? 10.2))} foobar) nil]

  As you can see, the preferred syntax for providing
  type hints to schema's defrecord', fn', and defn' macros
  is to follow each element, argument, or function name with a
  :- schema.  Symbols without schemas default to a schema of Any.
  In Clojure, class (e.g., java.lang.String) and primitive schemas
  (long, double) are also propagated to tag metadata to ensure
  you get the type hinting and primitive behavior you ask for.

  If you don't like this style, standard Clojure-style
  typehints are also supported:

  (fn-schema (fn' [^String x]))
  ==> (Fn Any [java.lang.String])

  You can directly type hint a symbol as a class, primitive,
  protocol, or simple schema.  For complex schemas, due to Clojure's
  rules about ^, you must enclose the schema in a {:s schema}
  map like so:

  (fn-schema (fn' [^{:s [String]} x]))
  (Fn Any [java.lang.String])

  (We highly prefer the :- syntax to this abomination, however.)
  See the docstrings of defrecord', fn', and defn' for more
  details about how to use these macros."
  (:require [clojure.string :as str]
            [clojure.data :as data]
            [stch.schema.util :as util])
  (:use [stch.util :only [indexed]]))

(defmacro ^:private try-catchall
  "A variant of try-catch that catches all exceptions in
  client (non-compilation) code. Does not (yet) support
  finally, and does not need or want an exception class."
  [& body]
  (let [try-body (butlast body)
        [catch sym & catch-body :as catch-form] (last body)]
    (assert (= catch 'catch))
    (assert (symbol? sym))
    `(try ~@try-body (~'catch Throwable ~sym ~@catch-body))))

(defmacro error!
  "Generate a cross-platform exception in client
  (non-compilation) code."
  ([s]
   `(throw (RuntimeException. ~(with-meta s `{:tag java.lang.String}))))
  ([s m]
   `(throw (clojure.lang.ExceptionInfo. ~(with-meta s `{:tag java.lang.String}) ~m))))

(defmacro safe-get
  "Like get but throw an exception if not found.
  A macro just to work around cljx function
  placement restrictions. Only valid in client
  (non-compilation) code."
  [m k]
  `(let [m# ~m k# ~k]
     (if-let [pair# (find m# k#)]
       (val pair#)
       (error! (format "Key %s not found in %s" k# m#)))))

(defmacro assert!
  "Like assert, but throws a RuntimeException and
  takes args to format. Only for use in client-code."
  [form & format-args]
  `(when-not ~form
     (error! (format ~@format-args))))

(defmacro ^:private assert-c!
  "Like assert! but throws a RuntimeException and takes
  args to format. Only for use during compilation."
  [form & format-args]
  `(when-not ~form
     (throw (RuntimeException. (format ~@format-args)))))

(defmacro validation-error
  [schema value expectation & [fail-explanation]]
  `(stch.schema.util/error
    (util/->ValidationError ~schema ~value (delay ~expectation) ~fail-explanation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for processing and normalizing element/argument schemas in defrecord' and (de)fn'

(defn- maybe-split-first [pred s]
  (if (pred (first s))
    [(first s) (next s)]
    [nil s]))

(defn- looks-like-a-protocol-var?
  "There is no 'protocol?'in Clojure, so here's
  a half-assed attempt."
  [v]
  (and (var? v)
       (map? @v)
       (= (:var @v) v)
       (:on @v)))

(defn- fix-protocol-tag [env tag]
  (or (when (symbol? tag)
        (when-let [v (resolve env tag)]
          (when (looks-like-a-protocol-var? v)
            `(protocol (deref ~v)))))
      tag))

(def ^:private primitive-sym?
  '#{float double boolean byte char short int long
     floats doubles booleans bytes chars shorts ints longs objects})

(defn- valid-tag? [env tag]
  (and (symbol? tag) (or (primitive-sym? tag)
                         (class? (resolve env tag)))))

(defn- normalized-metadata
  "Take an object with optional metadata, which may
  include a :tag and/or explicit :schema/:s/:s?/:tag
  data, plus an optional explicit schema, and normalize the
  object to have a valid Clojure :tag plus a :schema field.
  :s? is deprecated."
  [env imeta explicit-schema]
  (let [{:keys [tag s s? schema]} (meta imeta)]
    (assert-c! (< (count (remove nil? [s s? schema explicit-schema])) 2)
               "Expected single schema, got meta %s, explicit %s" (meta imeta) explicit-schema)
    (let [schema (fix-protocol-tag
                  env
                  (or s schema (when s? `(Option ~s?)) explicit-schema tag `Any))]
      (with-meta imeta
        (-> (or (meta imeta) {})
            (dissoc :tag :s :s? :schema)
            (util/assoc-when :schema schema
                             :tag (let [t (or tag schema)]
                                    (when (valid-tag? env t)
                                      t))))))))

(defn- extract-schema-form
  "Pull out the schema stored on a thing."
  [symbol]
  (let [s (:schema (meta symbol))]
    (assert-c! s "%s is missing a schema" symbol)
    s))

(defn- extract-arrow-schematized-element
  "Take a nonempty seq, which may start like [a ...] or
  [a :- schema ...], and return a list of
  [first-element-with-schema-attached rest-elements]."
  [env s]
  (assert (seq s))
  (let [[f & more] s]
    (if (= :- (first more))
      [(normalized-metadata env f (second more)) (drop 2 more)]
      [(normalized-metadata env f nil) more])))

(defn- process-arrow-schematized-args
  "Take an arg vector, in which each argument is followed
  by an optional :- schema, and transform into an ordinary
  arg vector where the schemas are metadata on the args."
  [env args]
  (loop [in args out []]
    (if (empty? in)
      out
      (let [[arg more] (extract-arrow-schematized-element env in)]
        (recur more (conj out arg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for schematized fn/defn

(defn- split-rest-arg [env bind]
  (let [[pre-& [_ rest-arg :as post-&]] (split-with #(not= % '&) bind)]
    (if (seq post-&)
      (do (assert-c! (= (count post-&) 2) "& must be followed by a single binding" (vec post-&))
        (assert-c! (or (symbol? rest-arg)
                       (and (vector? rest-arg)
                            (not-any? #{'&} rest-arg)))
                   "Bad & binding form: currently only bare symbols and vectors supported" (vec post-&))
        [(vec pre-&)
         (if (vector? rest-arg)
           (with-meta (process-arrow-schematized-args env rest-arg) (meta rest-arg))
           rest-arg)])
      [bind nil])))

(defn- single-arg-schema-form
  [rest? [index arg]]
  `(~(if rest? `optional `one)
     ~(extract-schema-form arg)
     ~(if (symbol? arg)
        `'~arg
        `'~(symbol (str (if rest? "rest" "arg") index)))))

(defn- simple-arglist-schema-form
  [rest? regular-args]
  (mapv (partial single-arg-schema-form rest?)
        (indexed regular-args)))

(defn- rest-arg-schema-form
  [arg]
  (let [s (extract-schema-form arg)]
    (if (= s `Any)
      (if (vector? arg)
        (simple-arglist-schema-form true arg)
        [`Any])
      (do
        (assert-c! (vector? s)
                   "Expected seq schema for rest args, got %s" s)
        s))))

(defn- input-schema-form
  [regular-args rest-arg]
  (let [base (simple-arglist-schema-form false regular-args)]
    (if rest-arg
      (vec (concat base (rest-arg-schema-form rest-arg)))
      base)))

(defn- apply-prepost-conditions
  "Replicate pre/post condition logic from clojure.core/fn."
  [body]
  (let [[conds body] (maybe-split-first #(and (map? %) (next body)) body)]
    (concat (map (fn [c] `(assert ~c)) (:pre conds))
            (if-let [post (:post conds)]
              `((let [~'% (do ~@body)]
                  ~@(map (fn [c] `(assert ~c)) post)
                  ~'%))
              body))))

(defn- process-fn-arity
  "Process a single (bind & body) form, producing an
  output tag, schema-form, and arity-form which has
  asserts for validation purposes added that are executed
  when turned on, and have very low overhead otherwise.
  tag? is a prospective tag for the fn symbol based on
  the output schema. schema-bindings are bindings to
  lift eval outwards, so we don't build the schema every
  time we do the validation."
  [env fn-name output-schema-sym bind-meta [bind & body]]
  (assert-c! (vector? bind) "Got non-vector binding form %s" bind)
  (when-let [bad-meta (seq (filter (or (meta bind) {}) [:tag :s? :s :schema]))]
    (throw (RuntimeException. (str "Meta not supported on bindings, put on fn name" (vec bad-meta)))))
  (let [original-arglist bind
        bind (with-meta (process-arrow-schematized-args env bind) bind-meta)
        [regular-args rest-arg] (split-rest-arg env bind)
        input-schema-sym (gensym "input-schema")
        enable-validation (not (:never-validate (meta fn-name)))
        input-checker-sym (gensym "input-checker")
        output-checker-sym (gensym "output-checker")]
    {:schema-binding [input-schema-sym (input-schema-form regular-args rest-arg)]
     :more-bindings (when enable-validation
                      [input-checker-sym `(checker ~input-schema-sym)
                       output-checker-sym `(checker ~output-schema-sym)])
     :arglist bind
     :raw-arglist original-arglist
     :arity-form
     (if enable-validation
       (let [bind-syms (vec (repeatedly (count regular-args) gensym))
             rest-sym (when rest-arg (gensym "rest"))
             metad-bind-syms (with-meta (mapv #(with-meta %1 (meta %2)) bind-syms bind) bind-meta)]
         (list
          (if rest-arg
            (into metad-bind-syms ['& rest-sym])
            metad-bind-syms)
          `(let [validate# ~(if (:always-validate (meta fn-name))
                              `true
                              `(.get_cell ~'ufv__))]
             (when validate#
               (let [args# ~(if rest-arg
                              `(list* ~@bind-syms ~rest-sym)
                              bind-syms)]
                 (when-let [error# (~input-checker-sym args#)]
                   (error! (format "Input to %s does not match schema: %s"
                                   '~fn-name (pr-str error#))
                           {:schema ~input-schema-sym :value args# :error error#}))))
             (let [o# (let ~(into (vec (interleave (map #(with-meta % {}) bind) bind-syms))
                                  (when rest-arg [rest-arg rest-sym]))
                        ~@(apply-prepost-conditions body))]
               (when validate#
                 (when-let [error# (~output-checker-sym o#)]
                   (error! (format "Output of %s does not match schema: %s"
                                   '~fn-name (pr-str error#))
                           {:schema ~output-schema-sym :value o# :error error#})))
               o#))))
       (cons bind body))}))

(defn- process-fn-
  "Process the fn args into a final tag proposal,
  schema form, schema bindings, and fn form."
  [env name fn-body]
  (let [output-schema (extract-schema-form name)
        output-schema-sym (gensym "output-schema")
        bind-meta (or (when-let [t (:tag (meta name))]
                        (when (primitive-sym? t)
                          {:tag t}))
                      {})
        processed-arities (map (partial process-fn-arity env name output-schema-sym bind-meta)
                               (if (vector? (first fn-body))
                                 [fn-body]
                                 fn-body))
        schema-bindings (map :schema-binding processed-arities)
        fn-forms (map :arity-form processed-arities)]
    {:outer-bindings (vec (concat
                           `[^stch.schema.util.PSimpleCell ~'ufv__ stch.schema.util/use-fn-validation]
                           [output-schema-sym output-schema]
                           (apply concat schema-bindings)
                           (mapcat :more-bindings processed-arities)))
     :arglists (map :arglist processed-arities)
     :raw-arglists (map :raw-arglist processed-arities)
     :schema-form `(make-fn-schema ~output-schema-sym ~(mapv first schema-bindings))
     :fn-body fn-forms}))

(defn- parse-arity-spec
  [spec]
  (assert-c! (vector? spec) "An arity spec must be a vector")
  (let [[init more] ((juxt take-while drop-while) #(not= '& %) spec)
        fixed (mapv (fn [i s] `(one ~s '~(symbol (str "arg" i)))) (range) init)]
    (if (empty? more)
      fixed
      (do (assert-c! (and (= (count more) 2) (vector? (second more)))
                     "An arity with & must be followed by a single sequence schema")
        (into fixed (second more))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public: miscellaneous macros and helpers

(defmacro Fn
  "Produce a function schema from an output schema
  and a list of arity input schema specs, each of which
  is a vector of argument schemas, ending with an optional
  '& more-schema' specification where more-schema must
  be a sequence schema.

  Currently function schemas are purely descriptive;
  there is no validation except for functions defined directly
  by fn' or defn'."
  ([]
   `(make-fn-schema ~'Any [~(parse-arity-spec '[& [Any]])]))
  ([output-schema & arity-schema-specs]
   `(make-fn-schema ~output-schema ~(mapv parse-arity-spec arity-schema-specs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public: schematized defrecord

(defmacro defrecord'
  "Define a record with a schema.

  In addition to the ordinary behavior of defrecord,
  this macro produces a schema for the Record, which
  will automatically be used when validating instances of
  the Record class:

  (defrecord' FooBar
    [foo :- Int
     bar :- String])

  (stch.schema.util/class-schema FooBar)
  ==> (record user.FooBar {:foo Int, :bar java.lang.String})

  (check FooBar (FooBar. 1.2 :not-a-string))
  ==> {:foo (not (integer? 1.2)), :bar (not (instance? java.lang.String :not-a-string))}

  See (doc stch.schema) for details of the :- syntax
  for record elements.

  Moreover, optional arguments extra-key-schema? and
  extra-validator-fn? can be passed to augment the record schema.
  - extra-key-schema is a map schema that defines validation
  for additional key-value pairs not in the record base
  (the default is to not allow extra mappings).
  - extra-validator-fn? is an additional predicate that
  will be used as part of validating the record value.

  The remaining opts+specs (i.e., protocol and interface
  implementations) are passed through directly to defrecord.

  Finally, this macro replaces Clojure's map->name constructor
  with one that is more than an order of magnitude faster
  (as of Clojure 1.5), and provides a new strict-map->name
  constructor that throws or drops extra keys not in the
  record base."
  {:arglists '([name field-schema extra-key-schema? extra-validator-fn? & opts+specs])}
  [name field-schema & more-args]
  (let [[extra-key-schema? more-args] (maybe-split-first map? more-args)
        [extra-validator-fn? more-args] (maybe-split-first (complement symbol?) more-args)
        field-schema (process-arrow-schematized-args &env field-schema)]
    `(do
       (let [bad-keys# (seq (filter #(required-key? %)
                                    (keys ~extra-key-schema?)))]
         (assert! (not bad-keys#) "extra-key-schema? can not contain required keys: %s"
                  (vec bad-keys#)))
       (when ~extra-validator-fn?
         (assert! (fn? ~extra-validator-fn?) "Extra-validator-fn? not a fn: %s"
                  (class ~extra-validator-fn?)))
       (defrecord ~name ~field-schema ~@more-args)
       (util/declare-class-schema!
        ~name
        (util/assoc-when
         (record
          ~name
          (merge ~(into {}
                        (for [k field-schema]
                          [(keyword (clojure.core/name k))
                           (do (assert-c! (symbol? k)
                                          "Non-symbol in record binding form: %s" k)
                             (extract-schema-form k))]))
                 ~extra-key-schema?))
         :extra-validator-fn ~extra-validator-fn?))
       ~(let [map-sym (gensym "m")]
          `(defn ~(symbol (str 'map-> name))
             ~(str "Factory function for class " name ", taking a map of keywords to field values, but not 400x"
                   " slower than ->x like the clojure.core version")
             [~map-sym]
             (let [base# (new ~(symbol (str name))
                              ~@(map (fn [s] `(get ~map-sym ~(keyword s))) field-schema))
                   remaining# (dissoc ~map-sym ~@(map keyword field-schema))]
               (if (seq remaining#)
                 (merge base# remaining#)
                 base#))))
       ~(let [map-sym (gensym "m")]
          `(defn ~(symbol (str 'strict-map-> name))
             ~(str "Factory function for class " name ", taking a map of keywords to field values.  All"
                   " keys are required, and no extra keys are allowed.  Even faster than map->")
             [~map-sym & [drop-extra-keys?#]]
             (when-not (or drop-extra-keys?# (= (count ~map-sym) ~(count field-schema)))
               (error! (format "Record has wrong set of keys: %s"
                                     (data/diff (set (keys ~map-sym))
                                                ~(set (map keyword field-schema))))))
             (new ~(symbol (str name))
                  ~@(map (fn [s] `(safe-get ~map-sym ~(keyword s))) field-schema)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public: schematized functions

(defmacro fn'
  "fn' : defn' :: clojure.core/fn : defn

  See (doc defn) for details.

  Additional gotchas and limitations:
  - Like defn', the output schema must go on the fn name.
  If you want an output schema, your function must have a name.
  - Unlike defn', the function schema is stored in metadata on the fn.
  Clojure's implementation for metadata on fns currently produces a
  wrapper fn, which will decrease performance and negate the benefits
  of primitive type hints compared to clojure.core/fn."
  [& fn-args]
  (let [[name more-fn-args] (if (symbol? (first fn-args))
                              (extract-arrow-schematized-element &env fn-args)
                              [(with-meta (gensym "fn") {:schema `Any}) fn-args])
        {:keys [outer-bindings schema-form fn-body]} (process-fn- &env name more-fn-args)]
    `(let ~outer-bindings
       (schematize-fn (fn ~name ~@fn-body) ~schema-form))))

(defn- format-arglist
  "Simplistic attempt to add comma separation
  to the arglist."
  [arglist]
  (let [num-args (count arglist)
        {num-ann ':-} (frequencies arglist)]
    (if (and (= (mod num-args 3) 0)
             (= num-ann (/ num-args 3)))
      (str "["
           (->> arglist
                (partition 3)
                (map #(str/join " " %))
                (str/join ", "))
           "]")
      arglist)))

(defmacro defn'
  "Like defn, except that schema-style typehints can
  be given on the argument symbols and on the function
  name (for the return value).

  You can call fn-schema on the defined function to
  get its schema back, or use with-fn-validation to
  enable runtime checking of function inputs and outputs.

  (defn' foo :- Num
    [x :- Int
     y :- Num]
    (* x y))

  (fn-schema foo)
  ==> (Fn java.lang.Number [Int java.lang.Number])

  (with-fn-validation (foo 1 2))
  ==> 2

  (with-fn-validation (foo 1.5 2))
  ==> Input to foo does not match schema: [(named (not (integer? 1.5)) x) nil]

  See (doc stch.schema) for details of the :- syntax for
  arguments and return schemas.

  The overhead for checking if run-time validation should be used is very
  small -- about 5% of a very small fn call.  On top of that, actual
  validation costs what it costs.

  You can also turn on validation unconditionally for this
  fn only by putting ^:always-validate metadata on the fn name.

  Gotchas and limitations:
  - The output schema always goes on the fn name, not the
  arg vector. This means that all arities must share the same
  output schema. Schema will automatically propagate primitive
  hints to the arg vector and class hints to the fn name,
  so that you get the behavior you expect from Clojure.
  - Schema metadata is only processed on top-level arguments.
  I.e., you can use destructuring, but you must put schema
  metadata on the top-level arguments, not the destructured variables.

  Bad:  (defn' foo [{:keys [x :- Int]}])
  Good: (defn' foo [{:keys [x]} :- {:x Int}])
  - Only a specific subset of rest-arg destructuring is supported:
  - & rest works as expected
  - & [a b] works, with schemas for individual elements parsed
  out of the binding, or an overall schema on the vector.
  - & {} is not supported.
  - Unlike defn, a final attr-map on multi-arity functions
  is not supported."
  [& defn-args]
  (let [[name more-defn-args] (extract-arrow-schematized-element &env defn-args)
        [doc-string? more-defn-args] (maybe-split-first string? more-defn-args)
        [attr-map? more-defn-args] (maybe-split-first map? more-defn-args)
        [f & more] defn-args
        {:keys [outer-bindings schema-form fn-body arglists raw-arglists]} (process-fn- &env name more-defn-args)]
    `(let ~outer-bindings
       (defn ~name
         ~(util/assoc-when
           (or attr-map? {})
           :doc (str
                 (str "Inputs: " (if (= 1 (count raw-arglists))
                                   (format-arglist (first raw-arglists))
                                   raw-arglists))
                 (when-let [ret (when (= (first more) :-) (second more))]
                   (str "\n  Returns: " ret))
                 (when doc-string? (str  "\n\n  " doc-string?)))
           :raw-arglists (list 'quote raw-arglists)
           :arglists (list 'quote arglists)
           :schema schema-form
           :tag (let [t (:tag (meta name))]
                  (when-not (primitive-sym? t)
                    t)))
         ~@fn-body)
       (util/declare-class-schema! (util/type-of ~name) ~schema-form))))

(defmacro letfn'
  [fnspecs# & body#]
  (list 'clojure.core/let
        (vec (interleave (map first fnspecs#)
                         (map macroexpand (map #(cons `fn' %) fnspecs#))))
        `(do ~@body#)))

(defmacro with-fn-validation
  "Execute body with input and ouptut schema validation
  turned on for all defn' and fn' instances."
  [& body]
  `(do
     (set-fn-validation! true)
     (try ~@body (finally (set-fn-validation! false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema protocol

(defprotocol Schema
  (walker [this]
    "Produce a function that takes [data], and either
    returns a walked version of data (by default, usually
    just data), or a util/ErrorContainer containing value
    that looks like the 'bad' parts of data with ValidationErrors
    at the leaves describing the failures.

    If this is a composite schema, should let-bind (subschema-walker
    sub-schema) for each subschema outside the returned fn.
    Within the returned fn, should break down data into constituents,
    call the let-bound subschema walkers on each component, and then
    reassemble the components into a walked version of the data
    (or an ErrorContainer describing the validaiton failures).

    Attempting to walk a value that already contains a
    util/ErrorContainer produces undefined behavior.

    User code should never call `walker` directly.
    Instead, it should call `start-walker` below.")
  (explain [this]
    "Expand this schema to a human-readable format suitable
    for pprinting, also expanding class schematas at the leaves.
    Example:

    user> (explain {:a Keyword :b [Int]} )
    {:a Keyword, :b [Int]}"))

;; Schemas print as their explains

(defmethod print-method stch.schema.Schema [s writer]
  (print-method (explain s) writer))
(prefer-method print-method stch.schema.Schema clojure.lang.IRecord)
(prefer-method print-method stch.schema.Schema java.util.Map)
(prefer-method print-method stch.schema.Schema clojure.lang.IPersistentMap)

(def ^:dynamic subschema-walker
  "The function to call within 'walker' implementations to
  create walkers for subschemas. Can be dynamically bound
  (using start-walker below) to create different walking behaviors.

  For the curious, implemented using dynamic binding rather
  than making walker take a subschema-walker as an argument
  because some behaviors (e.g. recursive schema walkers)
  seem to require mind-bending things like fixed-point
  combinators that way, but are simple this way."
  (fn [s]
    (error!
     (str "Walking is unsupported outside of start-walker; "
          "all composite schemas must eagerly bind subschema-walkers "
          "outside the returned walker."))))

(defn start-walker
  "The entry point for creating walkers.  Binds the provided
  walker to subschema-walker, then calls it on the provided schema.
  For simple validation, pass walker as sub-walker. More
  sophisticated behavior (coercion, etc), can be achieved by
  passing a sub-walker that wraps walker with additional behavior."
  [sub-walker schema]
  (binding [subschema-walker sub-walker]
    (subschema-walker schema)))

(defn checker
  "Compile an efficient checker for schema, which returns
  nil for valid values and error descriptions otherwise."
  [schema]
  (comp util/error-val (start-walker walker schema)))

(defn check
  "Return nil if x matches schema; otherwise, returns a
  value that looks like the 'bad' parts of x with
  ValidationErrors at the leaves describing the failures."
  [schema x]
  ((checker schema) x))

(defn validate
  "Throw an exception if value does not satisfy schema;
  otherwise, return value."
  [schema value]
  (when-let [error (check schema value)]
    (error! (format "Value does not match schema: %s" (pr-str error))
            {:schema schema :value value :error error}))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Platform-specific leaf Schemas

;; On the JVM, a Class itself is a schema. In JS, we treat functions as prototypes so any
;; function prototype checks objects for compatibility.

(extend-protocol Schema
  Class
  (walker [this]
    (let [class-walker
          (if-let [more-schema (util/class-schema this)]
            ;; do extra validation for records and such
            (subschema-walker more-schema)
            identity)]
      (fn [x]
        (or (when (not (instance? this x))
              (validation-error this x (list 'instance? this (util/value-name x))))
            (class-walker x)))))
  (explain [this]
    (if-let [more-schema (util/class-schema this)]
      (explain more-schema)
      (symbol (.getName ^Class this)))))


;; On the JVM, the primitive coercion functions (double, long, etc)
;; alias to the corresponding boxed number classes

(defmacro ^:private extend-primitive [cast-sym class-sym]
  `(extend-protocol Schema
     ~cast-sym
     (walker [this#]
             (subschema-walker ~class-sym))
     (explain [this#]
              (explain ~class-sym))))

(extend-primitive clojure.core$double Double)
(extend-primitive clojure.core$float Float)
(extend-primitive clojure.core$long Long)
(extend-primitive clojure.core$int Integer)
(extend-primitive clojure.core$short Short)
(extend-primitive clojure.core$char Character)
(extend-primitive clojure.core$byte Byte)
(extend-primitive clojure.core$boolean Boolean)

(extend-primitive clojure.core$doubles (Class/forName "[D"))
(extend-primitive clojure.core$floats (Class/forName "[F"))
(extend-primitive clojure.core$longs (Class/forName "[J"))
(extend-primitive clojure.core$ints (Class/forName "[I"))
(extend-primitive clojure.core$shorts (Class/forName "[S"))
(extend-primitive clojure.core$chars (Class/forName "[C"))
(extend-primitive clojure.core$bytes (Class/forName "[B"))
(extend-primitive clojure.core$booleans (Class/forName "[Z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cross-platform Schema leaves

;;; Any matches anything (including nil)

(defrecord AnythingSchema [_]
  ;; _ is to work around bug in Clojure where eval-ing defrecord with no fields
  ;; loses type info, which makes this unusable in schema-fn.
  ;; http://dev.clojure.org/jira/browse/CLJ-1196
  Schema
  (walker [this] identity)
  (explain [this] 'Any))

(def Any
  "Any value, including nil."
  (AnythingSchema. nil))

;;; eq (to a single allowed value)

(defrecord EqSchema [v]
  Schema
  (walker [this]
    (fn [x]
      (if (= v x)
        x
        (validation-error this x (list '= v (util/value-name x))))))
  (explain [this] (list 'eq v)))

(defn eq
  "A value that must be (= v)."
  [v]
  (EqSchema. v))

;;; enum (in a set of allowed values)

(defrecord EnumSchema [vs]
  Schema
  (walker [this]
    (fn [x]
      (if (contains? vs x)
        x
        (validation-error this x (list vs (util/value-name x))))))
  (explain [this] (cons 'enum vs)))

(defn enum
  "A value that must be = to some element of vs."
  [& vs]
  (EnumSchema. (set vs)))

;;; pred (matches all values for which p? returns truthy)

(defrecord PredicateSchema [p? pred-name]
  Schema
  (walker [this]
    (fn [x]
      (if-let [reason
               (try-catchall
                 (when-not (p? x) 'not)
                (catch e 'throws?))]
        (validation-error this x (list pred-name (util/value-name x)) reason)
        x)))
  (explain [this]
    (cond (= p? integer?) 'Int
          (= p? keyword?) 'Keyword
          :else (list 'pred pred-name))))

(defn pred-internal
  "A value for which p? returns true (and does not throw).
  Optional pred-name can be passed for nicer validation errors."
  [p? pred-name]
  (when-not (fn? p?)
    (error! (format "Not a function: %s" p?)))
  (PredicateSchema. p? pred-name))

(defmacro pred
  ([p?]
   `(pred-internal ~p? '~p?))
  ([p? pred-name]
   `(pred-internal ~p? ~pred-name)))

;;; protocol (which value must `satisfies?`)

(defn protocol-name [protocol]
  (-> protocol :p :var meta :name))

;; In cljs, satisfies? is a macro so we must precompile
;; (partial satisfies? p) and put it in metadata of the
;; record so that equality is preserved, along with the name.
(defrecord ProtocolSchema [p]
  Schema
  (walker [this]
    (fn [x]
      (if (satisfies? p x)
        x
        (validation-error this x (list 'satisfies? (protocol-name this) (util/value-name x))))))
  (explain [this] (list 'protocol (protocol-name this))))

;; The cljs version is protocol by necessity, since cljs `satisfies?` is a macro.
(defn protocol
  "A value that must satsify? protocol p."
  [p]
  (assert! (:on p) "Cannot make protocol schema for non-protocol %s" p)
  (ProtocolSchema. p))

;;; regex (validates matching Strings)

(extend-protocol Schema
  java.util.regex.Pattern
  (walker [this]
    (fn [x]
      (cond (not (string? x))
            (validation-error this x (list 'string? (util/value-name x)))

            (not (re-find this x))
            (validation-error this x (list 're-find
                                                  (explain this)
                                                  (util/value-name x)))
            :else x)))
  (explain [this]
    (symbol (str "#\"" this "\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple composite Schemas

;;; Option (nil)

(defrecord OptionSchema [schema]
  Schema
  (walker [this]
    (let [sub-walker (subschema-walker schema)]
      (fn [x]
        (when-not (nil? x)
          (sub-walker x)))))
  (explain [this] (list 'Option (explain schema))))

(defn Option
  "A value that must either be nil or satisfy schema."
  [schema]
  (OptionSchema. schema))

;;; named (schema elements)

(defrecord NamedSchema [schema name]
  Schema
  (walker [this]
    (let [sub-walker (subschema-walker schema)]
      (fn [x] (util/wrap-error-name name (sub-walker x)))))
  (explain [this] (list 'named (explain schema) name)))

(defn named
  "A value that must satisfy schema, and has a name
  for documentation purposes."
  [schema name]
  (NamedSchema. schema name))

;;; union (satisfies one or more schemas)

(defrecord UnionSchema [schemas]
  Schema
  (walker [this]
    (let [sub-walkers (mapv subschema-walker schemas)]
      (fn [x]
        (loop [sub-walkers (seq sub-walkers)]
          (if-not sub-walkers
            (validation-error
             this x
             (list 'some (list 'check '% (util/value-name x)) 'schemas))
            (let [res ((first sub-walkers) x)]
              (if-not (util/error? res)
                res
                (recur (next sub-walkers)))))))))
  (explain [this] (cons 'U (map explain schemas))))

(defn U
  "A value that must satisfy at least one schema in schemas."
  [& schemas]
  (UnionSchema. schemas))

;;; intersection (satisfies all schemas)

(defrecord IntersectionSchema [schemas]
  Schema
  (walker [this]
    (let [sub-walkers (mapv subschema-walker schemas)]
      ;; Intersection doesn't really have a clean semantics for non-identity walks, but we can
      ;; do something pretty reasonable and assume we walk in order passing the result
      ;; of each walk to the next, and failing at the first error
      (fn [x]
        (reduce
         (fn [x sub-walker]
           (if (util/error? x)
             x
             (sub-walker x)))
         x
         sub-walkers))))
  (explain [this] (cons 'I (map explain schemas))))

(defn I
  "A value that must satisfy every schema in schemas."
  [& schemas]
  (IntersectionSchema. schemas))

;;; conditional (choice of schema, based on predicates on the value)

(defrecord ConditionalSchema [preds-and-schemas]
  Schema
  (walker [this]
    (let [preds-and-walkers (mapv (fn [[pred schema]] [pred (subschema-walker schema)])
                                  preds-and-schemas)]
      (fn [x]
        (if-let [[_ match] (first (filter (fn [[pred]] (pred x)) preds-and-walkers))]
          (match x)
          (validation-error this x (list 'matches-some-condition? (util/value-name x)))))))
  (explain [this]
    (->> preds-and-schemas
         (mapcat (fn [[pred schema]] [pred (explain schema)]))
         (cons 'conditional))))

(defn conditional
  "Define a conditional schema.  Takes args like cond,
  (conditional pred1 schema1 pred2 schema2 ...),
  and checks the first schema where pred is true on the value.
  Unlike cond, throws if the value does not match any condition.
  :else may be used as a final condition in the place of (constantly true).
  More efficient than U, since only one schema must be checked."
  [& preds-and-schemas]
  (assert! (and (seq preds-and-schemas) (even? (count preds-and-schemas)))
                  "Expected even, nonzero number of args; got %s" (count preds-and-schemas))
  (ConditionalSchema. (for [[pred schema] (partition 2 preds-and-schemas)]
                        [(if (= pred :else) (constantly true) pred) schema])))

(defn schema-choice
  "If the predicate returns truthy, use the if-schema,
  otherwise use the else-schema."
  [pred if-schema else-schema]
  (conditional pred if-schema (constantly true) else-schema))

;;; Recursive schemas (Clojure only)
;; Supports recursively defined schemas by using the level
;; of indirection offered by by Clojure (but not ClojureScript) vars.

(defn- var-name [v]
  (let [{:keys [ns name]} (meta v)]
    (symbol (str (ns-name ns) "/" name))))

(defrecord RecursiveSchema [schema-var]
  Schema
  (walker [this]
    (let [a (atom nil)]
      (reset! a (start-walker
                 (let [old subschema-walker]
                   (fn [s] (if (= s this) #(@a %) (old s))))
                 @schema-var))))
  (explain [this]
    (list 'recursive (list 'var (var-name schema-var)))))

(defn recursive
  "Support for (mutually) recursive schemas by passing a var that points to a schema,
  e.g (recursive #'ExampleRecursiveSchema)."
  [schema-var]
  (when-not (var? schema-var)
    (error! (format "Not a var: %s" schema-var)))
  (RecursiveSchema. schema-var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Map Schemas

;; A map schema is itself a Clojure map, which can provide
;; value schemas for specific required
;; and optional keys, as well as a single, optional schema
;; for additional key-value pairs.

;; Specific keys are mapped to value schemas, and given as either:
;;  - (required-key k), a required key (= k)
;;  - a keyword, also a required key
;;  - (optional-key k), an optional key (= k)
;; For example, {:a Int (optional-key :b) String} describes
;; a map with key :a mapping to an integer, an optional key
;; :b mapping to a String, and no other keys.

;; There can also be a single additional key, itself a
;; schema, mapped to the schema for corresponding values,
;; which applies to all key-value pairs not covered by an explicit key.
;; For example, {Int String} is a mapping from integers to strings, and
;; {:a Int Int String} is a mapping from :a to an integer,
;; plus zero or more additional mappings from integers to strings.


;;; Definitions for required and optional keys, and single
;; entry validators.

(def +missing+
  "A sentinel value representing missing portions
  of the input data."
  ::missing)

(defrecord RequiredKey [k])

(defn required-key
  "A required key in a map."
  [k]
  (if (keyword? k)
    k
    (RequiredKey. k)))

(defn required-key?
  [ks]
  (or (keyword? ks)
      (instance? RequiredKey ks)))

(defrecord OptionalKey [k])

(defn optional-key
  "An optional key in a map."
  [k]
  (OptionalKey. k))

(defn optional-key? [ks]
  (instance? OptionalKey ks))

(defn explicit-schema-key [ks]
  (cond (keyword? ks) ks
        (instance? RequiredKey ks) (.-k ^RequiredKey ks)
        (optional-key? ks) (.-k ^OptionalKey ks)
        :else (error! (format "Bad explicit key: %s" ks))))

(defn specific-key? [ks]
  (or (required-key? ks)
      (optional-key? ks)))

(defn- explain-kspec [kspec]
  (if (specific-key? kspec)
    (if (keyword? kspec)
      kspec
      (list (cond (required-key? kspec) 'required-key
                  (optional-key? kspec) 'optional-key)
            (explicit-schema-key kspec)))
    (explain kspec)))

;; A schema for a single map entry. kspec is either a
;; keyword, required or optional key, or key schema.
;; val-schema is a value schema.
(defrecord MapEntrySchema [kspec val-schema]
  Schema
  (walker [this]
    (let [val-walker (subschema-walker val-schema)]
      (if (specific-key? kspec)
        (let [optional? (optional-key? kspec)
              k (explicit-schema-key kspec)]
          (fn [x]
            (cond (identical? +missing+ x)
                  (when-not optional?
                    (util/error [k 'missing-required-key]))

                  (not (= 2 (count x)))
                  (validation-error this x (list = 2 (list 'count (util/value-name x))))

                  :else
                  (let [[xk xv] x]
                    (assert (= xk k))
                    (let [vres (val-walker xv)]
                      (if-let [ve (util/error-val vres)]
                        (util/error [xk ve])
                        [xk vres]))))))
        (let [key-walker (subschema-walker kspec)]
          (fn [x]
            (if-not (= 2 (count x))
              (validation-error this x (list = 2 (list 'count (util/value-name x))))
              (let [out-k (key-walker (key x))
                    out-ke (util/error-val out-k)
                    out-v (val-walker (val x))
                    out-ve (util/error-val out-v)]
                (if (or out-ke out-ve)
                  (util/error [(or out-ke (key x)) (or out-ve 'invalid-key)])
                  [out-k out-v]))))))))
  (explain [this]
    (list 'map-entry
          (explain-kspec kspec)
          (explain val-schema))))

(defn map-entry [kspec val-schema]
  (MapEntrySchema. kspec val-schema))

;;; Implementation helper functions

(defn- find-extra-keys-schema [map-schema]
  (let [key-schemata (remove specific-key? (keys map-schema))]
    (assert! (< (count key-schemata) 2)
                    "More than one non-optional/required key schemata: %s"
                    (vec key-schemata))
    (first key-schemata)))

(defn- map-walker [map-schema]
  (let [extra-keys-schema (find-extra-keys-schema map-schema)
        extra-walker (when extra-keys-schema
                       (subschema-walker (apply map-entry (find map-schema extra-keys-schema))))
        explicit-schema (dissoc map-schema extra-keys-schema)
        explicit-walkers (into {} (for [[k v] explicit-schema]
                                    [(explicit-schema-key k)
                                     (subschema-walker (map-entry k v))]))
        err-conj (util/result-builder (constantly {}))]
    (when-not (= (count explicit-schema) (count explicit-walkers))
      (error! (format "Schema has multiple variants of the same explicit key: %s"
                                    (->> (keys explicit-schema)
                                         (group-by explicit-schema-key)
                                         vals
                                         (filter #(> (count %) 1))
                                         (apply concat)
                                         (mapv explain-kspec)))))
    (fn [x]
      (if-not (map? x)
        (validation-error map-schema x (list 'map? (util/value-name x)))
        (loop [x x explicit-walkers (seq explicit-walkers) out {}]
          (if-not explicit-walkers
            (reduce
             (if extra-walker
               (fn [out e]
                 (err-conj out (extra-walker e)))
               (fn [out [k _]]
                 (err-conj out (util/error [k 'disallowed-key]))))
             out
             x)
            (let [[wk wv] (first explicit-walkers)]
              (recur (dissoc x wk)
                     (next explicit-walkers)
                     (err-conj out (wv (or (find x wk) +missing+)))))))))))

(defn- map-explain [this]
  (into {} (for [[k v] this] (vec (next (explain (map-entry k v)))))))

(extend-protocol Schema
  clojure.lang.APersistentMap
  (walker [this] (map-walker this))
  (explain [this] (map-explain this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set schemas

;; A set schema is a Clojure set with a single element,
;; a schema that all values must satisfy.

(extend-protocol Schema
  clojure.lang.APersistentSet
  (walker [this]
    (assert! (= (count this) 1) "Set schema must have exactly one element")
    (let [sub-walker (subschema-walker (first this))]
      (fn [x]
        (or (when-not (set? x)
              (validation-error this x (list 'set? (util/value-name x))))
            (let [[good bad] ((juxt remove keep) util/error-val (map sub-walker x))]
              (if (seq bad)
                (util/error (set bad))
                (set good)))))))
  (explain [this] (set [(explain (first this))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequence Schemas

;; A sequence schema looks like [one* optional* rest-schema?].
;; one matches a single required element, and must be the
;; output of 'one' below. optional matches a single optional
;; element, and must be the output of 'optional' below.
;; Finally, rest-schema is any schema, which must match any
;; remaining elements. If optional elements are present, they
;; must be matched before the rest-schema is applied.

(defrecord One [schema optional? name])

(defn one
  "A single required element of a sequence
  (not repeated, the implicit default)."
  ([schema] (one schema nil))
  ([schema name]
   (One. schema false name)))

(defn optional
  "A single optional element of a sequence
  (not repeated, the implicit default)."
  ([schema] (optional schema nil))
  ([schema name]
   (One. schema true name)))

(defn- parse-sequence-schema [s]
  (let [[required more] (split-with #(and (instance? One %) (not (:optional? %))) s)
        [optional more] (split-with #(and (instance? One %) (:optional? %)) more)]
    (assert!
     (and (<= (count more) 1) (every? #(not (instance? One %)) more))
     "Sequence schema %s does not match [one* optional* rest-schema?]" s)
    [(concat required optional) (first more)]))

(extend-protocol Schema
  clojure.lang.APersistentVector
  (walker [this]
    (let [[singles multi] (parse-sequence-schema this)
          single-walkers (vec (for [^One s singles]
                                [s (subschema-walker (.-schema s))]))
          multi-walker (when multi (subschema-walker multi))
          err-conj (util/result-builder (fn [m] (vec (repeat (count m) nil))))]
      (fn [x]
        (or (when-not (or (nil? x) (sequential? x))
              (validation-error this x (list 'sequential? (util/value-name x))))
            (loop [single-walkers single-walkers
                   x x
                   index 0
                   out []]
              (if-let [[^One first-single single-walker]
                       (first single-walkers)]
                (if (empty? x)
                  (if (.-optional? first-single)
                    out
                    (err-conj out
                              (validation-error
                               (vec (map first single-walkers))
                               nil
                               (list* 'present?
                                      (for [[walker-index [^One single]]
                                            (indexed single-walkers)
                                            :while (not (.-optional? single))]
                                        (if-let [single-name (.-name single)]
                                          single-name
                                          (symbol (str "elem" (+ index walker-index)))))))))
                  (recur (next single-walkers)
                         (rest x)
                         (inc index)
                         (err-conj out
                                   (if-let [single-name (.-name first-single)]
                                     (util/wrap-error-name
                                       single-name
                                       (single-walker (first x)))
                                     (single-walker (first x))))))
                (cond multi
                      (reduce err-conj out (map multi-walker x))
                      (seq x)
                      (err-conj out (validation-error nil x (list 'has-extra-elts? (count x))))
                      :else out)))))))
  (explain [this]
    (let [[singles multi] (parse-sequence-schema this)]
      (vec
       (concat
        (for [^One s singles]
          (concat (list (if (.-optional? s) 'optional 'one)
                        (explain (.-schema s)))
                  (when-let [single-name (.-name s)]
                    (list single-name))))
        (when multi
          [(explain multi)]))))))

(defn pair
  "A schema for a pair of schemas and
  optionally their names."
  ([first-schema second-schema]
   (pair first-schema nil second-schema nil))
  ([first-schema first-name second-schema second-name]
   [(one first-schema first-name)
    (one second-schema second-name)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record Schemas

;; A Record schema describes a value that must have the correct
;; type, and its body must also satisfy a map schema.
;; An optional :extra-validator-fn can also be passed to do
;; additional validation.

(defrecord RecordSchema [klass schema]
  Schema
  (walker [this]
    (let [map-checker (subschema-walker schema)
          pred-checker (when-let [evf (:extra-validator-fn this)]
                         (subschema-walker (pred evf)))]
      (fn [r]
        (or (when-not (instance? klass r)
              (validation-error this r (list 'instance? klass (util/value-name r))))
            (let [res (map-checker r)]
              (if (util/error? res)
                res
                (let [pred-res (when pred-checker (pred-checker r))]
                  (if (util/error? pred-res)
                    pred-res
                    (merge r res)))))))))
  (explain [this]
    (list 'record (symbol (.getName ^Class klass))                                (explain schema))))

(defn record
  "A Record instance of type klass, whose
  elements match map schema 'schema'."
  [klass schema]
  (assert! (class? klass) "Expected record class, got %s" (util/type-of klass))
  (assert! (map? schema) "Expected map, got %s" (util/type-of schema))
  (RecordSchema. klass schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function Schemas

;; A function schema describes a function of one or more arities.
;; The function can only have a single output schema
;; (across all arities), and each input schema is a
;; sequence schema describing the argument vector.

;; Currently function schemas are purely descriptive,
;; and do not carry any validation logic.

(defn- explain-input-schema [input-schema]
  (let [[required more] (split-with #(instance? One %) input-schema)]
    (vec (concat (map #(explain (.-schema ^One %)) required)
                 (when (seq more)
                   ['& (mapv explain more)])))))

;; input-schemas sorted by arity
(defrecord FnSchema [output-schema input-schemas]
  Schema
  (walker [this]
    (fn [x]
      (if (ifn? x)
        x
        (validation-error this x (list 'ifn? (util/value-name x))))))
  (explain [this]
     (list* 'Fn (explain output-schema) (map explain-input-schema input-schemas))))

(defn- arity [input-schema]
  (if (seq input-schema)
    (if (instance? One (last input-schema))
      (count input-schema)
      Long/MAX_VALUE)
    0))

(defn make-fn-schema
  "A function outputting a value in output schema,
  whose argument vector must match one of input-schemas,
  each of which should be a sequence schema. Currently
  function schemas are purely descriptive; they validate
  against any function, regargless of actual input and
  output types."
  [output-schema input-schemas]
  (assert! (seq input-schemas) "Function must have at least one input schema")
  (assert! (every? vector? input-schemas) "Each arity must be a vector.")
  (assert! (apply distinct? (map arity input-schemas)) "Arities must be distinct")
  (FnSchema. output-schema (sort-by arity input-schemas)))

(defn set-fn-validation!
  "Globally turn on schema validation for all
  fn' and defn' instances."
  [on?]
  (.set_cell util/use-fn-validation on?))

(defn schematize-fn
  "Attach the schema to fn f at runtime,
  extractable by fn-schema."
  [f schema]
  (vary-meta f assoc :schema schema))

(defn ^FnSchema fn-schema
  "Produce the schema for a function defined
  with fn' or defn'."
  [f]
  (assert! (fn? f) "Non-function %s" (util/type-of f))
  (or (util/class-schema (util/type-of f))
      (safe-get (meta f) :schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in types

(def Int
  "Any integral number."
  (pred integer?))

(def Num
  "Any number."
  java.lang.Number)

(def Keyword
  "A Clojure keyword."
  clojure.lang.Keyword)

(def Symbol
  "A Clojure symbol."
  clojure.lang.Symbol)

(def Ratio
  "A Clojure ratio"
  clojure.lang.Ratio)

(def Atom
  "A Clojure atom"
  clojure.lang.Atom)

(def Regex
  "A regular expression."
  java.util.regex.Pattern)

(def Date
  "A Java date."
  java.util.Date)

(def UUID
  "A Java UUID."
  java.util.UUID)

(def Named
  "A Java string, Clojure symbol, or Clojure keyword."
  (U String Symbol Keyword))

(def Map
  "A Clojure map."
  (pred map?))

(def Set
  "A Clojure set."
  (pred set?))

(defn Vector
  "A Clojure vector of x's."
  ([] (pred vector?))
  ([x]
   (I (pred vector?) [x])))

(defn List
  "A Clojure list of x's."
  ([] (pred list?))
  ([x]
   (I (pred list?) [x])))

(defn Queue
  "A Clojure queue of x's."
  ([] clojure.lang.PersistentQueue)
  ([x]
   (I clojure.lang.PersistentQueue [x])))







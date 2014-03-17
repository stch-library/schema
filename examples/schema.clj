; Run in a REPL
(use 'stch.schema)

(def Data
  "A schema for a nested data type"
  {:a {:b String
       :c Int}
   :d [{:e Keyword
        :f [Num]}]})

(validate Data
          {:a {:b "abc"
               :c 123}
           :d [{:e :bc
                :f [12.2 13 100]}
               {:e :bc
                :f ["a"]}]})

(validate Data
          {:a {}})

(def Address {:city String
              :state String
              :country (Eq "US")})

(def User {:first-name String
           :last-name String
           (optional-key :address) Address})

(validate Address {:city "San Diego"
                   :state "CA"
                   :country "US"})

(validate User {:first-name "Billy"
                :last-name "Bob"})

(validate User {:first-name "Billy"
                :last-name "Bob"
                :address {:city "San Diego"
                          :state "CA"
                          :country "US"}})

(validate [String] ["Billy" "Bobby"])

(validate {Long {String Double}}
          {1 {"2" 3.0 "4" 5.0}})

(validate (Fn Int [Int]) inc)
(validate (Fn String [Map]) :name)
(validate (Fn Int [Int]) 3)

(defrecord' User
  [activated :- Date
   status :- Keyword
   likes :- [String]])

(validate User (->User #inst "2014-02-02" :active ["pizza" 35]))

(defn' hello-world :- String
  ([] "Hello, world")
  ([greeting :- String]
   (str greeting ", world")))

(explain User)
(explain (fn-schema hello-world))

(with-fn-validation
  (hello-world "Hi"))

(with-fn-validation
  (hello-world 5))

(validate Named "Hi")
(validate Named :Hi)

(validate [Num] [1 2 3])
(validate [Num] '(1 2 3))

(validate [(Pair String Int)]
          [["David" 35]["Billy" 37]])
(validate [(Pair String Int)]
          [["David" 35]["Billy" "37"]])
(validate [(Pair String Int)]
          [["David" 35]["Billy"]])
(validate [(Pair String Int)]
          [["David" 35][]])
(validate [(Pair String :name Int :age)]
          [["David" 35][]])
(validate [(Pair String :name Int :age)]
          [["David" 35][37 "Billy"]])

(validate [(One Int) Double] [1234 95.6 87.8 96.5])
(validate [(One Int) Double] ["1234" 95.6 87.8 96.5])
(validate [(One Int :id) (One String :alias)] [])
(validate [(Optional Int :id) (Optional String :name)] [])
(validate [(One Int) (One String)] [])
(validate [(One Int) (One String)] [1])
(validate [(One Int :id) (One String)] [])
(validate [(One Int) (One String) Keyword]
          ["hi" 5 'a])
(validate [Int] [])

(validate {:name String
           :status (Enumerate :active :inactive)}
          {:name "Bobby"
           :status :active})

(validate [(Predicate even?)] [2 4 6])
(validate [(Predicate even?)] [1 2 4 6])
(validate (I String (Predicate #(= (count %) 2)
                               "length of 2"))
          "hi")
(validate [(I (Predicate odd?) Long)] [1 3 5])
(validate [(I (Predicate odd?) Long)] [1 3 (int 5)])

(validate Symbol 'a)
(validate [Symbol] ['a 'b 'c])

(validate #"(?i)^[A-Z]+$" "Billy")

(defn' map-destruct :- (Vector String)
  [{:keys [likes]} :- {:likes [String]}]
  likes)

(with-fn-validation
  (map-destruct {:likes ["Pizza" "Hot Dogs"]}))

(with-fn-validation
  (map-destruct {:likes '("Pizza" "Hot Dogs")}))

(defn' seq-destruct :- String
  [[fst & rst] :- [String]]
  fst)

(with-fn-validation
  (seq-destruct ["hi" "hello"]))

(with-fn-validation
  (seq-destruct ["hi" :hello]))

(defn' rest-args :- (Vector String)
  [name :- String, age :- Int & likes :- [String]]
  (vec likes))

(with-fn-validation
  (rest-args "Billy" 35 "pizza" "hamburgers"))

(with-fn-validation
  (rest-args "Billy" 35 "pizza" :hamburgers))

(with-fn-validation
  (rest-args "Billy" 35 :pizza :hamburgers))

(defn' higher-order :- (Fn Int [Int])
  [f :- (Fn Int [Int])]
  f)

(with-fn-validation
  (higher-order inc))

; Not currently supported
(defn' keyword-args
  [url :- String & {:keys [follow-redirects]}])

(validate (List String) '("hi" "hello"))
(validate (List) '("Bob" :Billy))

(validate (Vector String) ["hi" "hello"])
(validate (Vector) ["Bob" :Billy])

(validate (Queue String)
          (conj clojure.lang.PersistentQueue/EMPTY "Billy"))
(validate (Queue String) '("Billy"))
(validate (Queue String)
          (conj clojure.lang.PersistentQueue/EMPTY 35))









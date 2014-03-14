(ns stch.schema-spec
  (:use [speclj.core]
        [stch.schema]
        [stch.util :only [empty-queue]]))

(def homogeneous-map
  {Keyword String})

(def Address {:city String
              :state String
              :country (Eq "US")})

(def User {:first-name String
           :last-name String
           (optional-key :address) Address})

(def Relationship
  {(required-key "husband") User
   (required-key "wife") User})

(defrecord' Employee
  [hired :- Date
   position :- Keyword
   roles :- [String]])

(def queue (conj empty-queue "Billy" "Bobby"))

(defn' simple-fn :- Int
  [x :- Int]
  (inc x))

(defn' map-destruct :- (Vector String)
  [{:keys [likes]} :- {:likes [String]}]
  likes)

(defn' seq-destruct :- String
  [[fst & rst] :- [String]]
  fst)

(defn' rest-args :- (Vector String)
  [name :- String, age :- Int & likes :- [String]]
  (vec likes))

(defn' multi-arity :- String
  ([url :- String] (multi-arity url))
  ([url :- String
    follow-redirects? :- Boolean]
   url))

(defn' higher-order :- (Fn Int [Int])
  [f :- (Fn Int [Int])]
  f)

(defn' any-fn :- (Fn)
  [f :- (Fn)]
  f)

#_(defn' loopy :- Int
  [x :- Int]
  (if (< x 5)
    (recur (inc x))
    x))

(defn' loop-recur :- Int
  [x :- Int]
  (loop [x x]
    (if (< x 5)
      (recur (inc x))
      x)))

(defn' ^:always-validate always-val :- Int
  [x :- Int]
  (inc x))

(defprotocol BasicMath
  (add [this y]))

(defrecord ANumber [x]
  BasicMath
  (add [this y] (+ x y)))

(defn invalid! [expect value]
  (should= (pr-str expect)
           (pr-str value)))

(describe "Maps"
  (context "homogeneous map"
    (it "valid"
      (should= {:first-name "Billy"
                :last-name "Bob"}
               (validate homogeneous-map
                         {:first-name "Billy"
                          :last-name "Bob"})))
    (it "invalid"
      (invalid! '{:first-name (not (instance? java.lang.String :Billy)),
                  :age (not (instance? java.lang.String 35))}
                (check homogeneous-map
                       {:first-name :Billy
                        :last-name "Bob"
                        :age 35}))))
  (it "required keyword keys"
    (should= {:city "San Diego"
              :state "CA"
              :country "US"}
             (validate Address
                       {:city "San Diego"
                        :state "CA"
                        :country "US"})))
  (it "required-key (String keys)"
    (should= {"husband" {:first-name "Billy"
                         :last-name "Bob"}
              "wife" {:first-name "Sally"
                      :last-name "Bob"}}
             (validate Relationship
                       {"husband" {:first-name "Billy"
                                   :last-name "Bob"}
                        "wife" {:first-name "Sally"
                                :last-name "Bob"}})))
  (it "optional-key (not present)"
    (should= {:first-name "Billy"
              :last-name "Bob"}
             (validate User
                       {:first-name "Billy"
                        :last-name "Bob"})))
  (it "optional-key (present)"
    (should= {:first-name "Billy"
              :last-name "Bob"
              :address {:city "San Diego"
                        :state "CA"
                        :country "US"}}
             (validate User
                       {:first-name "Billy"
                        :last-name "Bob"
                        :address {:city "San Diego"
                                  :state "CA"
                                  :country "US"}}))))

(describe "Sets"
  (it "non-empty"
    (should= #{1 2 3}
             (validate #{Int} #{1 2 3})))
  (it "empty"
    (should= #{}
             (validate #{Int} #{})))
  (it "invalid type"
    (invalid! '#{(not (integer? "3"))}
              (check #{Int} #{1 2 "3"}))))

(describe "Fn"
  (it "function"
    (should= inc
             (validate (Fn Int [Int]) inc)))
  (it "keyword"
    (should= :name
             (validate (Fn String [Map]) :name)))
  (it "not a function"
    (invalid! '(not (ifn? 3))
              (check (Fn Int [Int]) 3)))
  (it "invalid schema"
    (should-throw RuntimeException
                  "Function must have at least one input schema"
      (validate (Fn []) (fn [])))))

(describe "Built-in types"
  (context "Int"
    (it "Integer"
      (should= 3 (validate Int (int 3))))
    (it "Long"
      (should= 3 (validate Int 3)))
    (it "Double"
      (invalid! '(not (integer? 3.0))
                (check Int 3.0))))
  (context "Num"
    (it "Integer"
      (should= 3 (validate Num (int 3))))
    (it "Long"
      (should= 3 (validate Num 3)))
    (it "Double"
      (should= 3.0 (validate Num 3.0)))
    (it "String (fail)"
      (invalid! '(not (instance? java.lang.Number "3"))
                (check Num "3"))))
  (context "Keyword"
    (it "Keyword"
      (should= :name (validate Keyword :name)))
    (it "Symbol (fail)"
      (invalid! '(not (instance? clojure.lang.Keyword name))
                (check Keyword 'name))))
  (context "Symbol"
    (it "Symbol"
      (should= 'x (validate Symbol 'x)))
    (it "Keyword (fail)"
      (invalid! '(not (instance? clojure.lang.Symbol :name))
                (check Symbol :name))))
  (context "Ratio"
    (it "Ratio"
      (should= 2/3 (validate Ratio 2/3)))
    (it "Double (fail)"
      (invalid! '(not (instance? clojure.lang.Ratio 0.5))
                (check Ratio 0.5))))
  (context "Atom"
    (it "Atom"
      (should-not-throw (validate Atom (atom [1 2 3]))))
    (it "Vector (fail)"
      (invalid! '(not (instance? clojure.lang.Atom [1 2 3]))
                (check Atom [1 2 3]))))
  (context "Regex"
    (it "Regex literal"
      (should-not-throw
        (validate Regex #"[A-Z]")))
    (it "String (fail)"
      (invalid! '(not (instance? java.util.regex.Pattern "[A-Z]"))
                (check Regex "[A-Z]"))))
  (context "Date"
    (it "Date literal"
      (should= #inst "2014-02-02"
               (validate Date #inst "2014-02-02")))
    (it "String (fail)"
      (invalid! '(not (instance? java.util.Date "2014-02-02"))
                (check Date "2014-02-02"))))
  (context "UUID"
    (it "UUID literal"
      (should= #uuid "127965a6-8db9-11e3-94b5-425861b86ab6"
               (validate UUID #uuid "127965a6-8db9-11e3-94b5-425861b86ab6")))
    (it "String (fail)"
      (invalid! '(not (instance? java.util.UUID a-java.lang.String))
                (check UUID "127965a6-8db9-11e3-94b5-425861b86ab6"))))
  (context "Named"
    (it "Keyword"
      (should= :name (validate Named :name)))
    (it "String"
      (should= "name" (validate Named "name")))
    (it "Symbol"
      (should= 'name (validate Named 'name)))
    (it "Long (fail)"
      (should-throw Exception
                    "Value does not match schema: (not (some (check % 3) schemas))"
        (validate Named 3))))
  (context "Map"
    (it "Map"
      (should= {:name "Billy"}
               (validate Map {:name "Billy"})))
    (it "Set (fail)"
      (invalid! '(not (map? #{"Billy"}))
                (check Map #{"Billy"}))))
  (context "Set"
    (it "Set"
      (should= #{"Billy"}
               (validate Set #{"Billy"})))
    (it "Map (fail)"
      (invalid! '(not (set? {:name "Billy"}))
                (check Set {:name "Billy"}))))
  (context "Vector"
    (it "Vector of String"
      (should= ["Billy" "Bobby"]
               (validate (Vector String) ["Billy" "Bobby"])))
    (it "List of String (fail)"
      (invalid! '(not (vector? ("Billy" "Bobby")))
                (check (Vector String) '("Billy" "Bobby"))))
    (it "Vector of Int (fail)"
      (invalid! '[(not (integer? "Billy")) (not (integer? "Bobby"))]
                (check (Vector Int) ["Billy" "Bobby"]))))
    (it "Heterogeneous Vector"
      (should= ["Billy" :Bobby]
               (validate (Vector) ["Billy" :Bobby])))
  (context "List"
    (it "List of String"
      (should= '("Billy" "Bobby")
               (validate (List String) '("Billy" "Bobby"))))
    (it "Vector of String (fail)"
      (invalid! '(not (list? ["Billy" "Bobby"]))
                (check (List String) ["Billy" "Bobby"])))
    (it "List of Keyword (fail)"
      (invalid! '[(not (instance? java.lang.String :Billy))
                  (not (instance? java.lang.String :Bobby))]
                (check (List String) '(:Billy :Bobby)))))
    (it "Heterogeneous List"
      (should= '("Billy" :Bobby)
               (validate (List) '("Billy" :Bobby))))
  (context "Queue"
    (it "Queue of String"
      (should= queue
               (validate (Queue String) queue)))
    (it "Vector of String (fail)"
      (invalid! '(not (instance? clojure.lang.PersistentQueue ["Billy" "Bobby"]))
                (check (Queue String) ["Billy" "Bobby"])))
    (it "Queue of Int (fail)"
      (invalid! '[(not (integer? "Billy"))
                  (not (integer? "Bobby"))]
                (check (Queue Int) queue))))
    (it "Heterogeneous Queue"
      (should= queue
               (validate (Queue) queue))))

(describe "Sequences"
  (it "Vector"
    (should= [1 2 3]
             (validate [Num] [1 2 3])))
  (it "List"
    (should= '(1 2 3)
             (validate [Num] '(1 2 3))))
  (context "One (unnamed)"
    (it "Element present"
      (should= [1234 95.6]
               (validate [(One Int) Double]
                         [1234 95.6])))
    (it "Invalid type"
      (invalid! '[(not (integer? 57.0)) nil]
                (check [(One Int) Double]
                       [57.0 95.6])))
    (it "Not present"
      (invalid! '[(not (present? elem0 elem1))]
                (check [(One Int) (One String)] [])))
    (it "Not present (second element)"
      (invalid! '[nil (not (present? elem1))]
                (check [(One Int)
                        (One String)] [234]))))
  (context "One (named)"
    (it "Element present"
      (should= [1234 95.6]
               (validate [(One Int :id) Double]
                         [1234 95.6])))
    (it "Invalid type"
      (invalid! '[(named (not (integer? 57.0)) :id) nil]
                (check [(One Int :id) Double]
                       [57.0 95.6])))
    (it "Not present"
      (invalid! '[(not (present? :id :name))]
                (check [(One Int :id)
                        (One String :name)] [])))
    (it "Not present (second element)"
      (invalid! '[nil (not (present? :name))]
                (check [(One Int :id)
                        (One String :name)] [234]))))
  (context "Optional (unnamed)"
    (it "Both elements not present"
      (should= []
               (validate [(Optional Int)
                          (Optional String)] [])))
    (it "Second element not present"
      (should= [1234]
               (validate [(Optional Int)
                          (Optional String)]
                         [1234])))
    (it "Both elements present"
      (should= [1234 "Billy"]
               (validate [(Optional Int)
                          (Optional String)]
                         [1234 "Billy"])))
    (it "First element not present"
      (invalid! '[(not (integer? "Billy"))]
                (check [(Optional Int)
                        (Optional String)] ["Billy"])))
    (it "Invalid types"
      (invalid! '[(not (integer? "1234"))
                  (not (instance? java.lang.String :Billy))]
                (check [(Optional Int)
                        (Optional String)] ["1234" :Billy]))))
  (context "Optional (named)"
    (it "Both elements not present"
      (should= []
               (validate [(Optional Int :id)
                          (Optional String :name)] [])))
    (it "Second element not present"
      (should= [1234]
               (validate [(Optional Int :id)
                          (Optional String :name)]
                         [1234])))
    (it "Both elements present"
      (should= [1234 "Billy"]
               (validate [(Optional Int :id)
                          (Optional String :name)]
                         [1234 "Billy"])))
    (it "First element not present"
      (invalid! '[(named (not (integer? "Billy")) :id)]
                (check [(Optional Int :id)
                        (Optional String :name)] ["Billy"])))
    (it "Invalid types"
      (invalid! '[(named (not (integer? "1234")) :id)
                  (named (not (instance? java.lang.String :Billy)) :name)]
                (check [(Optional Int :id)
                        (Optional String :name)] ["1234" :Billy]))))
  (context "Pair"
    (it "unnamed"
      (should= [["David" 35] ["Billy" 37]]
               (validate [(Pair String Int)]
                         [["David" 35]["Billy" 37]])))
    (it "unnamed (fail)"
      (invalid! '[[nil (not (integer? "35"))] nil]
                (check [(Pair String Int)]
                       [["David" "35"]["Billy" 37]])))
    (it "named"
      (should= [["David" 35] ["Billy" 37]]
               (validate [(Pair String :name Int :age)]
                         [["David" 35]["Billy" 37]])))
    (it "named (invalid type)"
      (invalid! '[nil [(named (not (instance? java.lang.String 37)) :name)
                       (named (not (integer? "Billy")) :age)]]
               (check [(Pair String :name Int :age)]
                         [["David" 35][37 "Billy"]])))
    (it "named (not present)"
      (invalid! '[nil [(not (present? :name :age))]]
                (check [(Pair String :name Int :age)]
                       [["David" 35][]])))))

(describe "Predicate"
  (it "even?"
    (validate [(Predicate even?)] [2 4 6]))
  (it "even? (fail)"
    (invalid! '[(not (even? 1)) nil nil nil]
              (check [(Predicate even?)] [1 2 4 6]))))

(describe "Intersection"
  (it "pred and class"
    (should= [1 3 5]
             (validate [(I (Predicate odd?) Long)] [1 3 5])))
  (it "pred and class (fail)"
    (invalid! '[nil nil (not (instance? java.lang.Long 5))]
              (check [(I (Predicate odd?) Long)] [1 3 (int 5)]))))

(describe "Union"
  (context "String or Keyword"
    (it "String"
      (should= "Billy"
               (validate (U String Keyword) "Billy")))
    (it "Keyword"
      (should= :Billy
               (validate (U String Keyword) :Billy))))
  (it "String or Keyword (fail)"
    (should-throw Exception
                  "Value does not match schema: (not (some (check % Billy) schemas))"
      (validate (U String Keyword) 'Billy))))

(describe "Regex"
  (it "regex literal"
    (should= "Billy"
             (validate #"(?i)^[A-Z]+$" "Billy")))
  (it "regex literal (fail)"
    (invalid! '(not (re-find #"(?i)^[A-Z]+$" "Billy Bob"))
              (check #"(?i)^[A-Z]+$" "Billy Bob"))))

(describe "Enumerate"
  (it "Enumerate of Keyword"
    (should= {:name "Bobby"
             :status :active}
             (validate {:name String
                        :status (Enumerate :active :inactive)}
                       {:name "Bobby"
                        :status :active})))
  (it "Enumerate of Keyword (fail)"
    (invalid! '{:status (not (#{:inactive :active} :suspended))}
              (check {:name String
                      :status (Enumerate :active :inactive)}
                     {:name "Bobby"
                      :status :suspended}))))

(describe "Eq"
  (it "success"
    (should= "US"
             (validate (Eq "US") "US")))
  (it "fail"
    (invalid! '(not (= "US" :US))
              (check (Eq "US") :US))))

(describe "Protocol"
  (it "success"
    (should= (->ANumber 3)
             (validate (Protocol BasicMath)
                       (->ANumber 3))))
  (it "fail"
    (should-throw Exception
                  "Value does not match schema: (not (satisfies? BasicMath \"3\"))"
      (validate (Protocol BasicMath)
                "3"))))

(describe "Record"
  (it "success"
    (should= (->ANumber 3)
             (validate (Record ANumber {:x Int})
                       (->ANumber 3))))
  (it "fail"
    (should-throw Exception
                  "Value does not match schema: {:x (not (integer? :3))}"
                  (validate (Record ANumber {:x Int})
                            (->ANumber :3)))))

(describe "Option"
  (it "present"
    (should= "Billy"
             (validate (Option String) "Billy")))
  (it "not present"
    (should-be-nil
      (validate (Option String) nil)))
  (it "invalid type"
    (invalid! '(not (instance? java.lang.String :Billy))
              (check (Option String) :Billy))))

(describe "defn'"
  (it "map destruct"
    (should= ["Pizza" "Hot Dogs"]
             (with-fn-validation
               (map-destruct {:likes ["Pizza" "Hot Dogs"]}))))
  (it "map destruct (fail)"
    (should-throw Exception
                  "Output of map-destruct does not match schema: (not (vector? a-clojure.lang.PersistentList))"
      (with-fn-validation
        (map-destruct {:likes '("Pizza" "Hot Dogs")}))))
  (it "seq destruct"
    (should= "hi"
             (with-fn-validation
               (seq-destruct ["hi" "hello"]))))
  (it "seq destruct (fail)"
    (should-throw Exception
                  "Input to seq-destruct does not match schema: [(named [nil (not (instance? java.lang.String :hello))] arg0)]"
      (with-fn-validation
        (seq-destruct ["hi" :hello]))))
  (it "rest args"
    (should= ["pizza" "hamburgers"]
             (with-fn-validation
               (rest-args "Billy" 35 "pizza" "hamburgers"))))
  (it "rest args (invalid type)"
    (should-throw Exception
                  "Input to rest-args does not match schema: [nil nil nil (not (instance? java.lang.String :hamburgers))]"
      (with-fn-validation
        (rest-args "Billy" 35 "pizza" :hamburgers))))
  (it "higher order"
    (should= inc
             (with-fn-validation
               (higher-order inc))))
  (it "higher order (fail)"
    (should-throw Exception
                  "Input to higher-order does not match schema: [(named (not (ifn? 3)) f)]"
      (with-fn-validation
        (higher-order 3))))
  (it "any fn"
    (should= :name
             (with-fn-validation
               (any-fn :name))))
  #_(it "recur with implicit loop"
    (should= 5 (with-fn-validation
                 (loopy 3))))

  (it "loop/recur"
    (should= 5 (with-fn-validation
                 (loop-recur 3))))
  (it "always validate"
    (should-throw Exception
                  "Input to always-val does not match schema: [(named (not (integer? \"3\")) x)]"
      (always-val "3")))
  (context "explain"
    (it "single arity"
      (should= '(Fn Int [Int])
               (explain (fn-schema simple-fn))))
    (it "multiple arity"
      (should= '(Fn java.lang.String
                    [java.lang.String]
                    [java.lang.String java.lang.Boolean])
               (explain (fn-schema multi-arity))))
    (it "higher order"
      (should= '(Fn (Fn Int [Int]) [(Fn Int [Int])])
               (explain (fn-schema higher-order))))
    (it "any fn"
      (should= '(Fn (Fn Any [& [Any]]) [(Fn Any [& [Any]])])
               (explain (fn-schema any-fn))))))

(describe "defrecord'"
  (it "new record"
    (should= (->Employee #inst "2014-02-02"
                         :programmer
                         ["QA" "developement"])
             (validate Employee
                       (->Employee #inst "2014-02-02"
                                   :programmer
                                   ["QA" "developement"]))))
  (it "invalid"
    (invalid! '{:roles [nil (not (instance? java.lang.String 3))]}
              (check Employee
                     (->Employee #inst "2014-02-02"
                                 :programmer
                                 ["QA" 3])))))

(describe "fn'"
  (let [f (fn' increment :- Int
            [n :- Int]
            (inc n))]
    (list
      (context "validate"
        (it "named with return type"
          (should= 2
                   (with-fn-validation
                     (f 1))))
        (it "invalid"
          (should-throw Exception
                        "Input to increment does not match schema: [(named (not (integer? \"Billy\")) n)]"
                        (with-fn-validation
                          (f "Billy")))))
      (context "explain"
        (it "named with return type"
          (should= '(Fn Int [Int])
                   (explain (fn-schema f))))))))

(describe "letfn'"
  (letfn' [(x :- Num [] 1)
           (y :- String [n :- Num]
              (str n))]
    (list
      (context "validate"
        (it "success"
          (should= "1" (with-fn-validation
                         (y (x)))))
        (it "invalid"
          (should-throw Exception
                        "Input to y does not match schema: [(named (not (instance? java.lang.Number \"1\")) n)]"
            (with-fn-validation
              (y "1")))))
      (context "explain"
        (it "input and output types"
          (should= '(Fn java.lang.String [java.lang.Number])
                   (explain (fn-schema y))))))))












(defproject stch-library/schema "0.3.2"
  :description
  "Alternative Schema implementation.
  Clojure only at the moment."
  :url "https://github.com/stch-library/schema"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [stch-library/util "0.1.1"]]
  :profiles {:dev {:dependencies [[speclj "3.0.2"]]}}
  :plugins [[speclj "3.0.2"]
            [codox "0.6.7"]]
  :codox {:src-dir-uri "https://github.com/stch-library/schema/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :test-paths ["spec"])

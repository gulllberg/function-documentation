(ns function-documentation.core
  (:require [ysera.test :refer [is= is is-not]]))

(defn function-source?
  "Determines if the given string is the source code of function-name"
  {:test (fn []
           (is (function-source? "(defn foo [])" "foo"))
           (is-not (function-source? "(defn foo [])" "bar"))
           (is-not (function-source? "(defn foo [" "foo"))
           (is-not (function-source? "defn foo [])" "foo")))}
  [file-string function-name]
  (and (clojure.string/starts-with? file-string (str "(defn " function-name))
       (= (count (re-seq #"\(" file-string))
          (count (re-seq #"\)" file-string)))))

;; Depends on how you use defn. Linting might be needed to check this.
(defn get-function-source
  {:test (fn []
           (is= (get-function-source "sjdhsk (defn foo \"docstring\" [] bar)"
                                     "foo")
                "(defn foo \"docstring\" [] bar)")
           (is= (get-function-source "(defn bar) (defn foo \"docstring\" [] bar)"
                                     "foo")
                "(defn foo \"docstring\" [] bar)")
           (is= (get-function-source "(defn foo)"
                                     "bar")
                nil)
           (is= (get-function-source "(def foo)"
                                     "foo")
                nil))}
  [file-string function-name]
  (let [start-index (clojure.string/index-of file-string (str "(defn " function-name))]
    (if-not start-index
      (println "Did not find function :'(")
      (loop [end-index (clojure.string/index-of file-string ")" start-index)]
        (cond
          (function-source? (subs file-string start-index (inc end-index))
                            function-name)
          (subs file-string start-index (inc end-index))

          (not (nil? (clojure.string/index-of file-string ")" (inc end-index))))
          (recur (clojure.string/index-of file-string ")" (inc end-index)))

          :default
          (println "Did not find function :("))))))
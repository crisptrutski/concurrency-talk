(ns talk.-1_clj
  (:require
    [clojure.core.reducers :as r]
    [clojure.string :as str]))

;; syntax

(defn add "docstring" [a b] (+ a b))                        ;; default to prefix. { => (
(defn div [{:keys [num denom]}] (/ num denom))              ;; destructuring
(div {:num 14 :denom 2})                                    ;; f(10, 3), ( is on the outside

;; immutable

(def x {:a -1})
(def y (assoc x :b 2))
(def z (dissoc y :a))

;; laziness

(range 10)
(take 15 (range))

(def fib (lazy-cat [0 1] (map + fib (rest fib))))
(take 15 fib)

;; high-order functions

(->> (take 15 fib)
     (map inc)
     (filter odd?)
     (reduce +))

;; one level higher - let's debug

(defn p [label op & args]
  (let [result (apply op args)]
    (println label result)
    result))

(comment
  (->> (p :a take 15 fib)
       (p :b map inc)
       (p :c filter odd?)
       (p :d reduce +)))

;; higher-order code (macros)

(defmacro infix
  ([a] a)
  ([a op & rest]
   `(~op ~a (infix ~@rest))))

(infix 1 + 8 + 3 * 20)
(infix 1 + 8 * 3 - 20)

;; parallelism

(defn compute-9-times-table [i]
  (Thread/sleep 100)
  (* i 9))

(pmap compute-9-times-table (range 20))

;; fork/join

;; big bag of words
(def w
  (shuffle
    (mapcat (partial repeat 20)
            (str/split (slurp "lorem.txt") #"\s+"))))

;; boilerplate, note empty arity (seeds)

(defn count-words
  ([] {})
  ([freqs word]
   (assoc freqs word (inc (get freqs word 0)))))

(defn merge-counts
  ([] {})
  ([& m] (apply merge-with + m)))

;; the meat:

(defn word-frequency [words]
  (reduce count-words {} words))

(defn word-frequency-multi [words]
  (r/fold merge-counts count-words words))

(comment
  ;; sequential
  (take 10 (sort (time (word-frequency w))))
  ;; parallel (~ 2.7 times faster)
  (take 10 (sort (time (word-frequency-multi w)))))

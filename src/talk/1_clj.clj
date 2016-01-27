(ns talk.0_clj)

;; syntax

(defn f "docstring" [x y] (/ x y))      ;; default to prefix. { => (
(println (f 15 3))                      ;; println(f(10, 3)), ( => outside

;; immutable

(def x {:a 1})
(def y (assoc x :b 2))
(def z (dissoc y :a))

;; lazy

(def fib (lazy-cat [0 1] (map + fib (rest fib))))

(take 15 fib)

;; high-order functions

(->> (range 10)
     (map inc)
     (filter odd?)
     (reduce +))

;; one level higher - let's debug

(defn p [op & args]
  (let [result (apply op args)]
    (println result)
    result))

(->> (p range 10)
     (p map inc)
     (p filter odd?)
     (p reduce +))

;; higher-order code (macros)

(defmacro infix
  ([a] a)
  ([a op & rest]
   `(~op ~a (infix ~@rest))))

(infix 1 + 8 + 3 * 20)
(infix 0 + 8 * 3 - 20)                  ;; right associative

;; parallelism

(defn compute-9-times-table [i]
  (Thread/sleep 100)
  (* i 9))

(comment
  (pmap compute-9-times-table (range 20)))

(ns talk.5-atoms
  (:require
    [talk.helpers :refer :all]))

;; atoms

(def users (atom [{:name "predecessor"}]))
@users
(reset! users [])
(swap! users conj {:name "firstone"})
(swap! users conj {:name "successor"})

;; atoms with concurrency

(reduce + (range 50))

(let [cnt (atom 0)]
  ;; naive
  #_(doall (pmap (fn [x] (reset! cnt (+ x @cnt))) (range 50)))
  ;; atomic
  (doall (pmap (fn [x] (swap! cnt + x)) (range 49)))
  @cnt)

;; atom's weakness: synchronisation

(def x (atom (range 6)))
(def y (atom [\a \b \c \d \e \f]))

(do (pdotimes 6
      (let [[x1 & xs] @x
            [y1 & ys] @y]
        (reset! x (conj (vec xs) y1))
        (reset! y (conj (vec ys) x1))))
    [@x @y])

;; instead: refs

(def a (ref (range 6)))
(def b (ref [\a \b \c \d \e \f]))

(do (pdotimes 6
      (dosync
        (let [[a1 & as] @a
              [b1 & bs] @b]
          (ref-set a (conj (vec as) b1))
          (ref-set b (conj (vec bs) a1)))))
    [@a @b])

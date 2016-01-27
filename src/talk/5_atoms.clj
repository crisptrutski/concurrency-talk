(ns talk.5-atoms
  (:require
    [talk.helpers :refer :all]))

;; atoms

(def users (atom [{:name "predecessor"}]))
@users
(reset! users [])
(swap! users push {:name "firstone"})
(swap! users push {:name "successor"})

;; atoms with concurrency

(reduce + (range 50))

(let [cnt (atom 0)]
  ;; naive
  (doall (pmap (fn [x] (reset! cnt (+ x @cnt))) (range 50)))
  ;; atomic
  #_(doall (pmap (fn [x] (swap! cnt + x)) (range 50)))
  @cnt)

;; atom's weakness: synchronisation

(def x (atom (range 6)))
(def y (atom [\a \b \c \d \e \f]))

(comment
  [@x @y]
  (do (pdotimes 6
        (let [[x1 & xs] @x
              [y1 & ys] @y]
          (reset! x (conj (vec xs) y1))
          (reset! y (conj (vec ys) x1))))
      [@x @y]))

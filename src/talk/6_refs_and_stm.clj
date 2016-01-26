(ns talk.5-refs-and-stm
  (:require
    [talk.helpers :refer :all]))

;; atom reprise

(comment
  (def x (atom (range 6)))
  (def y (atom [\a \b \c \d \e \f]))

  (let [[x1 & xs] @x
        [y1 & ys] @y]
    (reset! x (conj (vec xs) y1))
    (reset! y (conj (vec ys) x1))))

;; fixed using refs

(def a (ref (range 6)))
(def b (ref [\a \b \c \d \e \f]))

(do (pdotimes 6
      (dosync
        (let [[a1 & as] @a
              [b1 & bs] @b]
          (ref-set a (conj (vec as) b1))
          (ref-set b (conj (vec bs) a1)))))
    [@a @b])

;; ** deeper example, with money at stake

;; (read and/or write consistency)

(def accounts (repeatedly 100 #(ref (rand-int 100))))

(defn transfer! []
  (let [a (nth accounts (rand-int 100))
        b (nth accounts (rand-int 100))
        m (rand-int 40)]
    (Thread/sleep (rand-int 2))
    (dosync
     (when (> @a m)
       (pp ".")
       (alter a - m)
       (Thread/sleep (rand-int 2))
       (pp ",")
       (commute b + m)))))

;; inconsistent
(defn print-total []
  (pp (reduce + (map deref accounts))))

;; consistent
(defn print-total-consistent []
  (dosync (print-total)))

;; static / all reads remained valid
(defn print-total-ensured []
  (dosync
   (run! ensure accounts)
   (print-total)))

(defn check-it! [^Runnable print-f]
  (let [threads (shuffle
                 (conj (for [_ (range 1e3)] (Thread. transfer!))
                       (Thread. print-f)))]
    (run! #(.start %) threads)
    (run! #(.join %) threads)))

(comment
  (print-total)
  (check-it! print-total)
  (check-it! print-total-consistent)
  (check-it! print-total-ensured))

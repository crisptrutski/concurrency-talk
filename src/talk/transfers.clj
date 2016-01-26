(ns talk.transfers
  (:require [talk.helpers :refer :all]))

(def accounts (repeatedly 102 #(ref (rand-int 100))))

(defn transfer! []
  (let [a (nth accounts (rand-int 100))
        b (nth accounts (rand-int 100))
        m (rand-int 40)]
    (dosync
     (when (> @a m)
       (pp "<")
       (alter a - m)
       (Thread/sleep (rand-int 2))
       (pp ">")
       (commute b + m)))))

(defn print-total []
  (pp (reduce + (map deref accounts))))

(defn print-total-consistent []
  (dosync (print-total)))

(defn print-total-ensured []
  (dosync
   (run! ensure accounts)
   (print-total)))

(defn check-it! [^Runnable print-f]
  (let [threads (shuffle
                 (conj (for [_ (range 1e3)] (Thread. transfer!))
                       (Thread. print-f)))]
    (run! #(.start %) threads)
    (run! #(.join %) threads)
    (println "\n")))

(comment
  (dotimes [_ 10] (check-it! print-total))
  (dotimes [_ 10] (check-it! print-total-consistent))
  (dotimes [_ 10] (check-it! print-total-ensured)))

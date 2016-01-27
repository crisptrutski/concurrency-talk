(ns talk.2-locks
  (:require
    [talk.helpers :refer :all]))

;; locks
;;;;;;;;

;; any reference or object can be used as a lock
(def o (Object.))

;; Demo 1: basic lock

(defn pick-lock [n]
  (println (str (inc n) " has started trying..."))
  (wait-max 500)
  (println (str (inc n) " has given up..."))
  (println "-----"))

(comment
  ;; overlapping
  (pdoseq [i (range 5)]
    (wait-max 99)
    (pick-lock i))

  ;; non overlapping
  (pdoseq [i (range 5)]
    (wait-max 100)
    (locking o
      (pick-lock i))))


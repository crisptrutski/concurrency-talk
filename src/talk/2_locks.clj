(ns talk.2-locks
  (:require
    [talk.helpers :refer :all])
  (:import java.util.concurrent.CountDownLatch))

;; locks
;;;;;;;;

;; any reference or object can be used as a lock (dynamic ftw)
(def o (Object.))

(defn wait-max
  "Wait from 0 to `max-ms` milliseconds"
  [max-ms]
  (Thread/sleep (rand max-ms)))

;; Demo 1: basic lock

(defn pick-lock [n]
  (println (str (inc n) " has started trying..."))
  (wait-max 500)
  (println (str (inc n) " has given up..."))
  (println "-----"))

(comment
  ;; overlapping
  (pdoseq [i (range 5)]
    (wait-max 100)
    (pick-lock i))

  ;; non overlapping
  (pdoseq [i (range 5)]
    (wait-max 100)
    (locking o
      (pick-lock i))))

;; Demo 2: re-entrant

(defn -step [n]
  (when (pos? n)
    (locking o
      (prn n)
      (-step (dec n)))))

(defn lock-step [n]
  (wait-max 100)
  (locking o
    (-step (inc n))
    (println "-----")))

(comment
  ;; non-overlapping, can re-lock
  (pdoseq [i (range 4)] (lock-step i)))


;; latches
;;;;;;;;;;

(defn compute-squares [n]
  ;; [] to see actual parallelism
  (let [xs (atom (sorted-set))
        latch (CountDownLatch. n)]
    (doseq [i (range n) :let [j (inc i)]]
      (.start
        (Thread.
          ^Runnable
          (fn []
            (Thread/sleep (rand-int 3))
            (swap! xs conj (* j j))
            (.countDown latch)))))
    (.await latch)
    @xs))

(comment
  (compute-squares 20)
  (dotimes [_ 5]
    (prn (compute-squares 20))))


(ns talk.cut
  (:require
    [clojure.string :as str]
    [clojure.core.reducers :as r]
    [talk.helpers :refer :all])
  (:import (java.util.concurrent CountDownLatch)))

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







;; locks and latches

;; Demo 2: re-entrant

(def o (Object.))

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
  (let [xs (atom (sorted-set) #_[])
        latch (CountDownLatch. n)]
    (pdoseq [i (range n) :let [j (inc i)]]
      (Thread/sleep (rand-int 3))
      (swap! xs conj (* j j))
      (.countDown latch))
    (.await latch)
    @xs))

(comment
  (compute-squares 20)
  (dotimes [_ 5]
    (prn (compute-squares 20))))



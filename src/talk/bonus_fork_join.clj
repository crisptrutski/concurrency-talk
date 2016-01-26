(ns talk.bonus-fork-join
  (:require
    [clojure.core.reducers :as r]
    [clojure.string :as str]))

;; some data

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
  ;; parallel
  (take 10 (sort (time (word-frequency-multi w)))))

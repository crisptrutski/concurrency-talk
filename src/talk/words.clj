(ns talk.words
  (:require
    [clojure.core.reducers :as r]
    [clojure.string :as str]))

(defn count-words
  ([] {})
  ([freqs word]
   (assoc freqs word (inc (get freqs word 0)))))

(defn merge-counts
  ([] {})
  ([& m] (apply merge-with + m)))

(defn words [text]
  (str/split text #"\s+"))

(defn word-frequency [words]
  (reduce count-words {} words))

(defn word-frequency-multi [words]
  (r/fold merge-counts count-words words))

(def w (shuffle (mapcat #(repeat 20 %) (words (slurp "lorem.txt")))))

(time (word-frequency w))

(time (word-frequency-multi w))


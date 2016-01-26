(ns talk.algae
  (:require
    [clojure.string :as str]))

(def state (agent "A"))

(def mapping {\A "AB" \B "A"})

(def expand #(str/escape % mapping))

(defn mutate [n wait]
  (dotimes [i n]
    (future
      (Thread/sleep (* i wait))
      (send state expand))))

(defn monitor []
  (while true
    (Thread/sleep 700)
    (println @state)))

(comment
  (mutate 15 1200)
  @state)

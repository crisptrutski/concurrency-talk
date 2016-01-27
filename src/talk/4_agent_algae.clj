(ns talk.4_agent_algae
  (:require
    [clojure.string :as str]
    [talk.helpers :refer :all]))

(def state (agent "A"))

(def mapping {\A "AB" \B "A"})

(def expand #(str/escape % mapping))

(comment
  @state
  ;; kick off the petri dish
  (future
    (dotimes [_ 13]
      (Thread/sleep 1000)
      (send state expand))))

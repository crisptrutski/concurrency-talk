(ns talk.3-promises
  (:require
    [talk.helpers :refer :all]
    [clojure.string :as str])
  (:import
    (java.util Date)))

;; async reads

;; ** promise: blockable container (can also use monad style)

(comment
  (do
    (def p (promise))
    (.start
      (Thread.
        ^Runnable
        (fn []
          (println "Reading...")
          (println "realized?" (realized? p))
          (println (str "Read: " @p))
          (println "realized?" (realized? p))
          (println "Done."))))
    (Thread/sleep 100)
    (println "Delivering...")
    (deliver p 1)))


;; ** delay: promise behind a thunk

(def delays
  (for [i (range 5)]
    (delay (prn 'pop 'sqrt (* i i))
           i)))

(comment
  ;; message only prints first time, result is cached
  @(nth delays 2))


;; ** future: eager background delay

(do
  (def f1 (future (Thread/sleep 200)))
  (def f2 (future (Thread/sleep 300)))
  (def f3 (future (Thread/sleep 100)))

  (def start (.getTime (Date.)))

  ;; block..
  @f1 @f2 @f3

  ;; blocke in parallel
  (println (- (.getTime (Date.)) start)))


;; ** agents (async write)

(def state (agent "A"))

(def mapping {\A "AB" \B "A"})

(defn expand [s]
  (Thread/sleep 1000)
  (str/escape s mapping))

(comment
  @state
  ;; kick off the petri dish
  (dotimes [_ 12]
    (send state expand)))


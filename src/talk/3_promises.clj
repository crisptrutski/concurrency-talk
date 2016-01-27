(ns talk.3-promises
  (:require
    [talk.helpers :refer :all])
  (:import
    (java.util Date)))

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
  (prn (- (.getTime (Date.)) start)))




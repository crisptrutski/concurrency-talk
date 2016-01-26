(ns talk.latch
  (:import java.util.concurrent.CountDownLatch))

(defn compute-squares [n]
  (let [xs (atom [] #_(sorted-set))
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
  (dotimes [_ 5]
    (prn (compute-squares 20))))


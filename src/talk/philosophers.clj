(ns talk.philosophers
  (:require
    [talk.helpers :refer :all]))

(def n 5)

(def forks
  (ref (into (sorted-set) (map inc) (range n))))

(def philosophers
  (for [i (range n)]
    (ref {:id (inc i)
          :status :sleeping
          :forks (sorted-set)})))

(defn roll-10? [max] (< (rand-int 10) max))

(defn- inc-stat [p key]
  (alter p update key (fnil inc 0)))

(defn view! [& _]
  (dosync
    (let [rows (cons {:id "--" :forks @forks}
                     (doall (map deref philosophers)))]
      (pp-table [:id :forks :status :waited :max-wait :feasted :tick] rows))))

;; logic

(defn both-forks? [p] (>= (count (:forks @p)) 2))

(defn take-fork! [p f]
  (when (@forks f)
    (ppp (str "p" (:id @p)) "takes" (str "f" f))
    (alter forks disj f)
    (alter p update :forks #(conj % f))))

(defn drop-fork! [p]
  (when-let [f (first (:forks @p))]
    (ppp (str "p" (:id @p)) "drops" (str "f" f))
    (alter p update :forks #(disj % f))
    (alter forks conj f)))

(defmulti next-status! (fn [state _] state))

(defmethod next-status! :default [_ p]
  (if (roll-10? 8) :thinking (next-status! :hungry p)))

(defmethod next-status! :hungry [_ p]
  (if (both-forks? p)
    :eating
    (do (or (and (take-fork! p (first @forks))
                 (take-fork! p (first @forks))
                 (next-status! :hungry p))
            (do (drop-fork! p)
                (inc-stat p :waited)
                :hungry)))))

(defmethod next-status! :eating [_ p]
  (if (roll-10? 6) :eating (next-status! :finished p)))

(defmethod next-status! :finished [_ p]
  (when (both-forks? p)
    (alter p update :max-wait #(max (or % 0) (:waited @p -1)))
    (alter p dissoc :waited)
    (inc-stat p :feasted))
  (if (seq (:forks @p))
    (do (drop-fork! p)
        (drop-fork! p)
        (next-status! :finished p))
    :thinking))

(defn tick! [p]
  (inc-stat p :tick)
  (let [next (next-status! (:status @p) p)]
    (alter p assoc :status next)))

(defn lock-step []
  (dotimes [_ 100]
    (pdoseq [p philosophers]
      (dosync (tick! p)))
    (view!)))

(defn chaos []
  (pdoseq [p (shuffle (take (* n 200) (cycle philosophers)))]
    (Thread/sleep (rand (+ 10 n)))
    (when (roll-10? 1)
      (view!))
    (dosync (tick! p)))
  (view!))

(comment
  (lock-step)
  (chaos)
  (reset-agent))


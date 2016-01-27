(ns talk.7-dining-philosophers
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
  (let [rows (dosync (cons {:id "--" :forks @forks} (doall (map deref philosophers))))]
    (pp-table [:id :forks :status :waited :max-wait :feasted :tick] rows)))

;; logic

(defn both-forks? [p] (>= (count (:forks @p)) 2))

(defn p-forks [p] [(:id @p) (inc (mod (:id @p) n))])

(defn next-fork [p]
  (let [[a b] (sort (p-forks p))]
    (if ((:forks @p) a) b a)))

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
  (if (roll-10? 8) :thinking :hungry))

(defmethod next-status! :hungry [_ p]
  (if (both-forks? p)
    :eating
    (do (when-not (take-fork! p (@forks (next-fork p)))
          (inc-stat p :waited))
        :hungry)))

(defmethod next-status! :eating [_ p]
  (if (roll-10? 6) :eating :finished))

(defmethod next-status! :finished [_ p]
  (when (both-forks? p)
    (alter p update :max-wait #(max (or % 0) (:waited @p -1)))
    (alter p dissoc :waited)
    (inc-stat p :feasted))
  (if (seq (:forks @p))
    (do (drop-fork! p) :finished)
    :sleeping))

(defn tick! [p]
  (inc-stat p :tick)
  (let [next (next-status! (:status @p) p)]
    (alter p assoc :status next)))

(defn tick-all! []
  (pdoseq [p philosophers]
    (dosync (tick! p))
    (view!)))

(defn fully-parallel []
  (pdoseq [p (shuffle (take (* n 200) (cycle philosophers)))]
    (Thread/sleep (rand (+ 10 n)))
    (when (roll-10? 1)
      (view!))
    (dosync (tick! p)))
  (view!))

(comment
  (view!)
  (do (dosync (tick! (first philosophers)))
      (view!))
  (tick-all!)
  (pdotimes 100 (tick-all!))
  (fully-parallel)
  (reset-agent))


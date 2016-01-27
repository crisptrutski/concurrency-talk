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

(defn view! [& _]
  (dosync
    (let [rows (cons {:id "--" :forks @forks}
                     (doall (map deref philosophers)))]
      (pp-table [:id :forks :status] rows))))

;; logic

(defn both-forks? [p] (>= (count (:forks @p)) 2))

;(defn- low-fork [p] (:id @p))

;(defn- high-fork [p] (inc (mod (:id @p) n)))

(defn- p-forks [p]
  (let [id (:id @p)]
    [id (inc (mod id n))]))

(defn- low-fork [p] (apply min (p-forks p)))

(defn- high-fork [p] (apply max (p-forks p)))

(defn next-fork [p forks]
  (let [low (low-fork p)]
    (or (@forks low)
        (and ((:forks @p) low)
             (high-fork p)))))

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

(defmethod next-status! :default [_ _]
  (if (roll-10? 8) :thinking :hungry))

(defmethod next-status! :hungry [_ p]
  (if (both-forks? p)
    :eating
    (do (when-let [f (next-fork p forks)]
          (take-fork! p f))
        :hungry)))

(defmethod next-status! :eating [_ _]
  (if (roll-10? 6) :eating :finished))

(defmethod next-status! :finished [_ p]
  (if (empty? (:forks @p))
    :thinking
    (do (drop-fork! p)
        :finished)))

(defn tick! [p]
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


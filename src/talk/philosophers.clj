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

;; Used for side effects in transactions
(def a (agent nil))

(defn roll-10? [max] (< (rand-int 10) max))

(defn- inc-stat [p key]
  (alter p update key (fnil inc 0)))

(defn view! [& _]
  (let [rows (dosync
               (cons {:id "--" :forks @forks}
                     (map deref philosophers)))]
    (pp-table [:id :forks :status] rows)))

(defn view-summary! []
  (let [ms (dosync (map deref philosophers))]
    (pp-table [:id :forks :status :waited :feasted :tick] ms)))

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
    (ppp "take" (:id @p) f)
    (alter forks disj f)
    (alter p update :forks #(conj % f))))

(defn drop-fork! [p]
  (when-let [f (first (:forks @p))]
    (ppp "drop" (:id @p) f)
    (alter p update :forks #(disj % f))
    (alter forks conj f)))

(defmulti next-status! (fn [state _] state))

(defmethod next-status! :default [_ _]
  (if (roll-10? 8) :thinking :hungry))

(defmethod next-status! :hungry [_ p]
  (if (both-forks? p)
    :eating
    (do (let [f (next-fork p forks)]
          (or (and f (take-fork! p f))
              (inc-stat p :waited)))
        :hungry)))

(defmethod next-status! :eating [_ _]
  (if (roll-10? 6) :eating :finished))

(defmethod next-status! :finished [_ p]
  (when (both-forks? p)
    (inc-stat p :feasted))
  (if (seq (:forks @p))
    (do (drop-fork! p)
        :finished)
    :thinking))

(defn tick! [p]
  (inc-stat p :tick)
  (let [next (next-status! (:status @p) p)]
    (alter p assoc :status next)))

(defn runz []
  (dotimes [_ 100]
    (doall (pmap #(dosync (tick! %))  philosophers))
    #_(await print-agent)
    (view!))
  (view-summary!)
  :done)

(comment
  (runz)
  (reset-agent))


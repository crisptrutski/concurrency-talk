(ns talk.philosophers
  (:require
    [talk.helpers :refer :all]))

(def n 5)

(def forks (ref (into (sorted-set) (map inc) (range n))))

(def philosophers (for [_ (range n)] (ref (sorted-set))))

;; Assign :id metadata to philosophers
(doseq [[p i] (map vector philosophers (range))]
  (alter-meta! p assoc :id (inc i)))

;; Used for side effects in transactions (eg metadata, logging)
(def a (agent nil))

(defmacro do-agent [& body]
  `(send a (fn [~'_] ~@body)))

(defn inc-meta [p k]
  (do-agent
    (when-not (= :tick k) (ppp (:id (meta p)) k))
    (alter-meta! p update k (fnil inc 0))))

(defn roll-10? [max] (< (rand-int 10) max))

(defn details [ref]
  (let [m (meta ref)]
    {:id (:id m "--")
     :status (:status m "--")
     :forks @ref}))

(defn view! [& _]
  (let [rows (dosync (map details (cons forks philosophers)))]
    (send a (fn [_] (pp-table [:id :forks :status] rows)))))

(defn view-meta! []
  (let [ms (dosync (map meta philosophers))]
    (send a (fn [_] (pp-table [:id :status :waited :feasted :tick] ms)))))

;; logic

(defn both-forks? [p] (>= (count @p) 2))

;(defn- low-fork [p] (:id (meta p)))
;(defn- high-fork [p] (inc (mod (:id (meta p)) n)))

(defn- p-forks [p]
  (let [id (:id (meta p))]
    [id (inc (mod id n))]))

(defn- low-fork [p] (apply min (p-forks p)))

(defn- high-fork [p] (apply max (p-forks p)))

(defn next-fork [p forks]
  (let [low (low-fork p)]
    (or (@forks low)
        (and (@p low)
             (high-fork p)))))

(defn take-fork! [p f]
  (when (@forks f)
    (alter forks disj f)
    (alter p conj f)))

(defn drop-fork! [p]
  (when-let [f (first @p)]
    (alter p #(disj % f))
    (alter forks conj f)))

(defmulti next-status! (fn [state _] state))

(defmethod next-status! :default [_ _]
  (if (roll-10? 8) :thinking :hungry))

(defmethod next-status! :hungry [_ p]
  (if (both-forks? p)
    :eating
    (do (let [f (next-fork p forks)]
          (or (and f (take-fork! p f))
              (inc-meta p :waited)))
        :hungry)))

(defmethod next-status! :eating [_ _]
  (if (roll-10? 6) :eating :finished))

(defmethod next-status! :finished [_ p]
  (when (both-forks? p)
    (inc-meta p :feasted))
  (if (seq @p)
    (do (drop-fork! p)
        :finished)
    :thinking))

(defn tick! [p]
  (alter-meta! p update :status #(do (inc-meta p :tick)
                                     (next-status! % p))))

(defn runz []
  (dotimes [_ 100]
    (doall (pmap #(do #_(do-agent (pp "<"))
                      (dosync (tick! %))
                      #_(do-agent (pp ">"))) philosophers))
    (await a)
    (view!)
    (await a))
  (view-meta!)
  :done)

(comment
  (runz))

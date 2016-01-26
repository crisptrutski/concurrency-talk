(ns talk.philosophers)

(def o *out*)

(defn pprn [& args]
  (binding [*out* o]
    (apply println args)))

(def a (agent nil))

(defn philo-meta! [p k f]
  (send a (fn [_]
            (pprn (:id (meta p)) k)
            (alter-meta! p update k f))))

(def n 5)
(def forks (ref (into (sorted-set) (map inc) (range n))))
(def philosophers (for [_ (range n)] (ref (sorted-set))))

(doseq [[p i] (map vector philosophers (range))]
  (alter-meta! p assoc :id (inc i)))

(defn roll-10? [max] (< (rand-int 10) max))

(defn details [ref]
  (let [m (meta ref)]
    {:id (:id m "--")
     :status (:status m "--")
     :forks @ref}))

(defn ready? [p] (>= (count @p) 2))

;(defn enough? [p forks]
;  (>= (+ (count @p) (count @forks)) 2))
;
;(defn next-fork [p forks]
;  (when (enough? p forks)
;    (last @forks)))

(defn next-fork [p forks]
  (let [id (:id (meta p))]
    (or (@forks id)
        (and (@p id) (inc (mod id n))))))

(defn take-fork [p f]
  (when (@forks f)
    (alter forks disj f)
    (commute p conj f)))

(defn take-a-fork [p]
  (or (ready? p)
      (when-let [f (next-fork p forks)]
        (take-fork p f))))

(defn drop-fork [p f]
  (when (@p f)
    (alter p #(disj % f))
    (commute forks conj f)))

(defn drop-a-fork [p]
  (doseq [f @p]
    (drop-fork p f)))

(defn view! [& _]
  (binding [*out* o]
    (clojure.pprint/print-table
      [:id :forks :status]
      (map details (cons forks philosophers)))))

(defmulti transition-philo (fn [state _] state))

(defmethod transition-philo :default [_ _]
  (if (roll-10? 8) :thinking :hungry))

(defmethod transition-philo :hungry [_ p]
  (if (ready? p)
    :eating
    (do (when-not (take-a-fork p)
          (philo-meta! p :waited (fnil inc 0)))
        :hungry)))

(defmethod transition-philo :eating [_ _]
  (if (roll-10? 6) :eating :finished))

(defmethod transition-philo :finished [_ p]
  (when (ready? p)
    (philo-meta! p :eaten (fnil inc 0)))
  (drop-a-fork p)
  (if (seq @p) :finished :thinking))

(defn update-philo [p]
  (alter-meta! p update :status #(transition-philo % p)))

(defn runz []
  (dotimes [_ 100]
    (doall (pmap #(dosync (update-philo %)) philosophers))
    (send a view!))

  (send a (fn [_]
            (binding [*out* o]
              (clojure.pprint/print-table
                [:id :status :waited :eaten]
                (map meta philosophers))))))

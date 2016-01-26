;; promise based abstraction

;; @ (read)

(do
   (def p (promise))

   (.start
      (Thread.
       (fn []
         (println "Run..")
         (println (str "Read: " @p))
         (println "Done.."))))

   (println "Delivering...")
   (Thread/sleep 100)
   (deliver p 1))

;; delay

(do
  (def delays (for [i (range 5)] (delay (prn 'pop 'sqrt (* i i)) i)))
  @(nth delays 2))

;; future

(do
  (def f1 (future (Thread/sleep 2000)))
  (def f2 (future (Thread/sleep 3000)))
  (def f3 (future (Thread/sleep 1000)))

  (def start (.getTime (java.util.Date.)))

  @f1
  @f2
  @f3

  (prn (- (.getTime (java.util.Date.)) start)))

;; show locks

(def o (Object.))

(defn wait-max [max-ms]
  (Thread/sleep (rand max-ms)))

;; show it's exclusive

(defn lock-picker [n]
  (future
    (wait-max 100)
    (locking o
      (println (str (inc n) " has started trying..."))
      (wait-max 500)
      (println (str (inc n) " has given up..."))
      (println "-----"))))

;; show it's re-entrant

(defn -lock-step [n]
  (when (pos? n)
    (locking o
      (prn n)
      (-lock-step (dec n)))))

(defn lock-step [n]
  (future
    (wait-max 100)
    (locking o
      (-lock-step (inc n))
      (println "-----"))))

(map deref
     (for [i (range 5)]
       (lock-picker i)))

(println "\n======\n")

(dotimes [i 5]
  (lock-step i))

;; parallelism

(map #(* % %) (range 50))
;; (0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361)

(pmap (fn [x] (Thread/sleep 100) (* x x)) (range 50))
;; not 5 seconds

(def a (agent nil))

(defn sync-prn [& args]
  (send-off a (fn [_] (apply prn args))))

;; straight up parallelism
(run! #(future (Thread/sleep 100) (sync-prn (* % %))) (range 50))




;; atoms [from ML, 1973]

(def users (atom [{:name "predecessor"}]))

(reset! users [])

(swap! users conj {:name "firstone"})
(swap! users conj {:name "successor"})

(prn @users)

;; atoms with concurrency

(reduce + (range 50))

(def cnt (atom 0))
(doall (pmap (fn [x] (reset! cnt (+ x @cnt))) (range 50)))
@cnt ;; probably not 1225
(reset! cnt 0)
(doall (pmap (fn [x] (swap! cnt + x)) (range 50)))
@cnt ;; 1225


;; atoms + concurrency + immutable data

;; lightweight rpg

;; add user
;; rename user
;; swap 1hp between users
;; remove dead users
;; shuffle users
;; include index of user


;; atom's weakness: synchronisation

;; prove random order
(def x (atom []))
(pmap #(swap! x conj %) (range 9))
;; randomised order

(def x (atom 10))
(def y (atom 0))

;; pyramid = 55

(pmap (fn [_] (swap! y + @x) (swap! x dec)) (range @x))
;; yay.. broken

(prn @x ":" @y)

(pmap (fn [_] (swap! y + (inc (swap! x dec)))) (range @x))
;; damn.. still works

(prn @x ":" @y)


(def x (ref 10))
(def y (ref 0))

(pmap (fn [_] (dosync (alter y + @x) (alter x dec))) (range @x))
;; works



;; example that's better i think

(def x (atom (range 6)))
(def y (atom [\a \b \c \d \e \f]))

(defn first-to-last [i]
  (let [[x1 & xs] @x
        [y1 & ys] @y]
    (reset! x (conj (vec xs) y1))
    (reset! y (conj (vec ys) x1))))

(pmap first-to-last (range 6))

;; [5 \a \b \d \e \f]
;; [0 1 2 2 3 4]

;; yay... screwed!

(def x (ref (range 6)))
(def y (ref [\a \b \c \d \e \f]))

(defn first-to-last [i]
  (dosync
    (let [[x1 & xs] @x
          [y1 & ys] @y]
      (ref-set x (conj (vec xs) y1))
      (ref-set y (conj (vec ys) x1)))))

(pmap first-to-last (range 6))

;; [\a \b \c \d \e \f]
;; [0 1 2 3 4 5]


;; agent: put value on implicit queue for identified node
;; agent: put function on implicit queue for value
;; csp: put value on explicit queue




;; laziness?

(def fib (cons 1 (cons 1 (lazy-seq (map + fib (rest fib))))))

;; fibs = 1 : 1 : next fibs
;;   where
;;     next (a:t@(b:_)) = (a+b) : next t








;; philosophers!

(def forks)
(def philosophers)

(defn inverse [m] (into {} (map (juxt val key)) m))
;; (inverse {:a 1 :b 2 :c 3 :d 1})
;; => {1 :d, 2 :b, 3 :c}

(defn setup! [n]
  (alter-var-root #'philosophers (constantly n))
  (alter-var-root #'forks (constantly (map #(ref {(inc %) nil}) (range (inc n))))))

(setup! 5)

(defn view! []
  (let [fs (map deref forks)
        grp (into (sorted-map) (map (fn [[k v]] [k (map key v)])) (group-by val (reduce merge fs)))]
    (doseq [p (concat [nil] (range 1 (inc philosophers)))]
      (println (or p "f") ":" (get grp p)))))

(defn pick-up [p f]
  (alter (nth forks (dec f))
         (fn [x]
           (if (get x f)
             x
             (assoc x f p)))))



;;


(def n 5)
(def forks (ref (into (sorted-set) (map inc) (range (inc n)))))
(def philosophers (map ref (repeat n nil)))

(defn state? [p]
  (:status (meta p)))

(defn take-fork [p f]
  (let [ok? (atom nil)]
    (alter forks (fn [forks?]
                   (reset! ok? (forks? f))
                   (disj forks? f)))
    (when @ok?
      (alter p conj f))))

(defn ready? [p]
  (>= (count @p) 2))

(defn take-forks [p]
  (or (ready? p)
      (let [fs (drop (count @p) @forks)]
        (take-fork p (last fs)))))

(defn drop-fork [p f]
  (let [ok? (atom nil)]
    (alter p
           (fn [fs]
             (reset! ok? (some #{f} fs))
             (remove #{f} fs)))
    (when @ok?
      (alter forks conj f))))

(defn drop-forks [p]
  (doseq [f @p]
    (drop-fork p f)))

(defn view! []
  (println "f" ":" @forks)
  (doseq [[i p] (map vector (range) philosophers)]
    (println (inc i) ":" @p (str "(" (name (or (state? p) :sleeping)) ")"))))


(defn update-philosopher [p]
  (alter-meta! p update :status
               (fn [s]
                 (case s
                   :eating (if (< (rand-int 10) 4)
                             (do (drop-forks p) :thinking)
                             :eating)
                   :hungry (do
                             (take-forks p)
                             (if (ready? p)
                               :eating
                               :hungry))
                   (if (< (rand-int 10) 8)
                     :thinking
                     :hungry)))))



(def a (agent nil))

(dotimes [i 100]
  (doall (pmap (fn [x] (dosync (update-philosopher x))) philosophers))
  (view!)
  (println "--"))

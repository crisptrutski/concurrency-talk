(ns talk.draf
  (:require
    [talk.helpers :refer :all])
  (:import
    (java.util Date)))

;; promise based abstraction

;; @ (read)

(do
  (def p (promise))

  (.start
    (Thread.
      ^Runnable
      (fn []
        (println "Reading...")
        (println (str "Read: " @p))
        (println "Done."))))
  (Thread/sleep 100)
  (println "Delivering...")
  (deliver p 1))

;; delay

(def delays
  (for [i (range 5)]
    (delay (prn 'pop 'sqrt (* i i))
           i)))

@(nth delays 2)

;; future

(do
  (def f1 (future (Thread/sleep 200)))
  (def f2 (future (Thread/sleep 300)))
  (def f3 (future (Thread/sleep 100)))

  (def start (.getTime (Date.)))

  @f1
  @f2
  @f3

  (prn (- (.getTime (Date.)) start)))

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

(pdoseq [i (range 5)] (lock-picker i))

;; show it's re-entrant

(defn -step [n]
  (when (pos? n)
    (locking o
      (prn n)
      (-step (dec n)))))

(defn lock-step [n]
  (wait-max 100)
  (locking o
    (-step (inc n))
    (println "-----")))

(pdoseq [i (range 5)] (lock-step i))

;; parallelism

(map #(* % %) (range 50))
;; (0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361)

(pmap (fn [x] (Thread/sleep 100) (* x x)) (range 50))
;; not 5 seconds



;; straight up parallelism
(run! #(future (Thread/sleep 100) (ppp (* % %))) (range 50))


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
;; probably not 1225
@cnt

(reset! cnt 0)
(doall (pmap (fn [x] (swap! cnt + x)) (range 50)))
;; 1225
@cnt


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


;; Mutually consuming queues

(def x (atom (range 6)))
(def y (atom [\a \b \c \d \e \f]))

(defn first-to-last [_]
  (let [[x1 & xs] @x
        [y1 & ys] @y]
    (reset! x (conj (vec xs) y1))
    (reset! y (conj (vec ys) x1))))

(pmap first-to-last (range 6))

[@x @y]

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

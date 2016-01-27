(ns talk.helpers
  (:require
    [clojure.pprint :as pp]))

(def push conj)

(def print-agent (agent nil))

(defmacro with-agent [& body]
  `(do (send print-agent (fn [~'_] ~@body))
       nil))

(def out *out*)

(defn pp [& args]
  (with-agent
    (binding [*out* out]
      (apply print args))))

(defn ppp [& args]
  (with-agent
    (binding [*out* out]
      (apply println args))))

(defn pp-table [&  args]
  (with-agent
    (binding [*out* out]
      (apply pp/print-table args))))


(defn reset-agent []
  (agent-error print-agent)
  (restart-agent print-agent nil))

(defmacro pdoseq [binding & body]
  `(let [ts# (for ~binding (Thread. ^Runnable #(do ~@body)))]
     (run! #(.start %) ts#)
     (run! #(.join %) ts#)))

(defmacro pdotimes [n & body]
  `(let [ts# (for [~'_ (range ~n)] (Thread. ^Runnable #(do ~@body)))]
     (run! #(.start %) ts#)
     (run! #(.join %) ts#)))

(ns talk.helpers
  (:require
    [clojure.pprint :as pp]))

(def print-agent (agent nil))

(defmacro with-agent [& body]
  `(send print-agent (fn [~'_] ~@body)))

(def o *out*)

(defn pp [& args]
  (with-agent
    (binding [*out* o]
      (apply print args))))

(defn ppp [& args]
  (with-agent
    (binding [*out* o]
      (apply println args))))

(defn pp-table [&  args]
  (with-agent
    (binding [*out* o]
      (apply pp/print-table args))))


(defn reset-agent []
  (agent-error print-agent)
  (restart-agent print-agent nil))

(defmacro pdoseq [binding & body]
  `(let [ts# (for ~binding (Thread. ^Runnable #(do ~@body)))]
     (run! #(.start %) ts#)
     (run! #(.join %) ts#)))

(ns talk.helpers
  (:require
    [clojure.pprint :as pp]))

(def o *out*)

(defn pp [& args]
  (binding [*out* o]
    (apply print args)))

(defn ppp [& args]
  (binding [*out* o]
    (apply println args)))

(defn pp-table [&  args]
  (binding [*out* o]
    (apply pp/print-table args)))

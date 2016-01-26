(ns talk.helpers)

(let [o *out*]
  (defn p [x]
    (binding [*out* o] (print x))))

(ns talk.clj)

(def fib (lazy-cat [0 1] (map + fib (rest fib))))

(take 15 fib)

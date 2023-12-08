(ns advent-of-code.day-8
  (:require [clojure.string :as s]))

(defn pt1 [i]
  (let [[a & b] (re-seq #"\w+" i)
        t (into
           {}
           (map (juxt first identity))
           (partition 3 b))]
    (loop [[i & o] (cycle a)
           [x l r] (t "AAA")
           s 0]
      (if (= x "ZZZ")
        s
        (recur o (t (if (= \L i) l r)) (inc s))))))

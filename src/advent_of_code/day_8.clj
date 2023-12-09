(ns advent-of-code.day-8
  (:require [clojure.string :as s]))

(defn parse [i]
  (let [[a & b] (re-seq #"\w+" i)
        t (into
           {}
           (map (juxt first identity))
           (partition 3 b))]
    [a t]))

(defn z [g p n]
  (loop [[i & o] (cycle p)
         [[_ _ x] l r] n
         s 0]
    (if (= x \Z)
      s
      (recur o (g (if (= \L i) l r)) (inc s)))))

(defn pt1 [i]
  (let [[a g] (parse i)]
    (z g a (g "AAA"))))

(defn pt2 [i]
  (let [[p g] (parse i)
        b (->> g vals (filter (fn [[[_ _ l]]] (= l \A))))
        x (fn [a b] (if (zero? b) a (recur b (mod a b))))
        y (fn [a b] (bigint (/ (* a b) (x a b))))]
    (reduce y (map #(z g p %) b))))



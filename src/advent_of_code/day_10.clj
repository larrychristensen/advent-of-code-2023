(ns advent-of-code.day-10
  (:require [clojure.string :as s]
            [advent-of-code.core :as c]))

(defn starting-location [graph]
  (first
   (for [i (range (count graph))
         j (range (count (first graph)))
         :let [c (get-in graph [i j])]
         :when (= c \S)]
     [i j])))

(declare connected-nodes)

(defn s-connected-nodes [graph [i j]]
  (filter
   (fn [[x y]]
     (and
      (not= \. (get-in graph [x y]))
      (let [connected (connected-nodes graph [x y])]
       ((set connected) [i j]))))
   [[(dec i) j] [(inc i) j] [i (dec j)] [i (inc j)]]))

(defn connected-nodes [graph [i j]]
  (when-let [c (get-in graph [i j])]
    (filter
     (fn [[i j]]
       (when-let [c (get-in graph [i j])]
         (not= c \.)))
     (case c
       \S (s-connected-nodes graph [i j])
       \| [[(dec i) j] [(inc i) j]]
       \- [[i (dec j)] [i (inc j)]]
       \L [[(dec i) j] [i (inc j)]]
       \J [[(dec i) j] [i (dec j)]]
       \7 [[(inc i) j] [i (dec j)]]
       \F [[(inc i) j] [i (inc j)]]))))

(defn steps-to-furthest-position-from-start [graph]
  (let [s-loc (starting-location graph)]
    (loop [[f & r] (map
                    (fn [[x y]]
                      [s-loc [x y] 1])
                    (connected-nodes graph s-loc))
           traversed #{}]
      (let [[src dest steps] f]
        (if (and (pos? steps)
                 (= \S (get-in graph dest)))
          (/ steps 2)
          (let [connected (sequence
                           (comp (remove
                                  (fn [[x y]]
                                    (traversed #{dest [x y]})))
                                 (map
                                  (fn [[x y]]
                                    [dest [x y] (inc steps)])))
                           (connected-nodes graph dest))]
            (recur (into r connected)
                   (conj traversed #{src dest}))))))))

(ns advent-of-code.day-11
  (:require [clojure.string :as s]))

(defn empty-columns [universe]
  (filter
   (fn [i]
     (every?
      (fn [line]
        (= \. (get line i)))
      universe))
   (-> universe first count range)))

(defn empty-rows [universe]
  (filter
   (fn [i]
     (every?
      (fn [c]
        (= \. c))
      (universe i)))
   (-> universe count range)))

(defn expand-row [empty-cols line]
  (apply str (sequence
              (comp
               (map-indexed
                (fn [i c]
                  (if (empty-cols i)
                    [c c]
                    [c])))
               cat)
              line)))

(defn expand-universe [universe]
  (let [empty-cols (set (empty-columns universe))
        empty-rows (set (empty-rows universe))]
    (into
     []
     (comp (map-indexed
            (fn [i line]
              (let [expanded (expand-row empty-cols line)]
                (if (empty-rows i)
                  [expanded expanded]
                  [expanded]))))
           cat)
     universe)))

(defn galaxy-positions [universe]
  (for [y (range (count universe))
        x (range (count (first universe)))
        :let [c (get-in universe [y x])]
        :when (= c \#)]
    [y x]))

(defn galaxy-distances [input expansion-rate]
  (let [universe (s/split-lines input)
        empty-cols (set (empty-columns universe))
        empty-rows (set (empty-rows universe))
        posns (galaxy-positions universe)
        pairs (set (for [g1 posns
                         g2 posns
                         :when (not= g1 g2)]
                     #{g1 g2}))]
    (reduce
     +
     (for [pair pairs]
       (let [[[y1 x1 :as g1] [y2 x2 :as g2]] (vec pair)
             xs (sort [x1 x2])
             ys (sort [y1 y2])
             empty-cols-between (count (filter
                                        (fn [x]
                                          (empty-cols x))
                                        (apply range xs)))
             empty-rows-between (count (filter
                                        (fn [y]
                                          (empty-rows y))
                                        (apply range ys)))
             v (+ (abs (apply - ys))
                  (abs (apply - xs))
                  (* (dec expansion-rate) empty-cols-between)
                  (* (dec expansion-rate) empty-rows-between))]
         v)))))

(defn part-1 [input]
  (galaxy-distances input 2))

(defn part-2 [universe]
  (galaxy-distances input 1000000))

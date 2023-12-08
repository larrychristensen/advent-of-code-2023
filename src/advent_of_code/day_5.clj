(ns advent-of-code.day-5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.set :as sets]))

(defn line-numbers [line]
  (map read-string (re-seq #"\d+" line)))

(defn parse-input [input]
  (let [[seeds & maps] (s/split input #"\n\n")]
    [(line-numbers seeds)
     (map
      (fn [m]
        (->> m
             s/split-lines
             (drop 1)
             (map line-numbers)
             (sort-by first)))
      maps)]))

(defn dest->src [value mappings]
  (or
   (some
    (fn [[dest src size]]
      (let [diff (- value dest)]
        (when (<= 0 diff (dec size))
          (+ src diff))))
    mappings)
   value))

(defn fill-ranges [ranges]
  (into
   [[0 (ffirst ranges) 0]]
   (mapcat (fn [[[s1 e1 :as r1] 
                 [s2]]]
             (if s2 
               (if (pos? (- s2 e1))
                 [r1 [e1 s2 0]]
                 [r1])
               [r1 [e1 Long/MAX_VALUE 0]])))
   (partition-all 2 1 ranges)))

(defn ranges-disjoint? [[s1 e1] [s2 e2]]
  (or (>= s1 e2)
      (>= s2 e1)))

(def ranges-intersect? (complement ranges-disjoint?))

(defn intersect-ranges [rs1 rs2]
  (for [[s1 e1 :as r1] rs1
        [s2 e2 offset2 :as r2] rs2
        :when (ranges-intersect? r1 r2)]
    [(max s1 s2) (min e1 e2) offset2]))

(defn offset-range [[s e] v]
  [(+ s v) (+ e v) v])

(defn mapping->range [[dest src size]]
  [src (+ src size) (- dest src)])

(defn sort-by-first [s]
  (sort-by first s))

(defn convert-mappings-to-ranges [mappings]
  (map
   (fn [m]
     (sort-by-first
      (map
       mapping->range
       m)))
   mappings))

(defn seed-numbers->ranges [seed-numbers]
  (sort-by-first
   (map
    (fn [[start size]]
      [start (+ start size)])
    (partition-all 2 seed-numbers))))

(defn offset-ranges [ranges]
  (map
   (fn [range]
     (offset-range range (get range 2)))
   ranges))

(defn ranges-min-start [ranges]
  (apply min (map first ranges)))

;; Starting with the seed ranges, intersects those with
;; the next set of ranges. It then offsets the resulting
;; ranges and recurses. When the final range is arrived
;; at, it just returns the min of range mins.
(defn min-location [input]
  (let [[seed-numbers mappings] (parse-input input)
        seed-ranges (seed-numbers->ranges seed-numbers)
        map-ranges (convert-mappings-to-ranges mappings)]
    (loop [src-range seed-ranges
           [map-range & rest-map-ranges] map-ranges]
      (if map-range
        (let [filled-ranges (fill-ranges map-range)
              intersected-ranges (intersect-ranges src-range
                                                   filled-ranges)]
          (recur
           (offset-ranges intersected-ranges)
           rest-map-ranges))
        (ranges-min-start src-range)))))

(ns advent-of-code.day-12
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(defn make-re-greedy-separators [group-sizes]
  (->> group-sizes
       (map (fn [size]
              (format "([\\?#]{%s}?)" size)))
       (interpose "([\\?\\.]+)")
       (apply str)
       (format "(?=(.*?)%s(.*?))")
       re-pattern))

(defn make-re-lazy-separators [group-sizes]
  (->> group-sizes
       (map (fn [size]
              (format "([\\?#]{%s}?)" size)))
       (interpose "([\\?\\.]+?)")
       (apply str)
       (format "(?=(.*?)%s(.*?))")
       re-pattern))

(defn query [regex line]
  (into #{}
        (re-seq regex line)))

(defn possible-arrangements [line]
  (let [[pattern group-sizes-str] (s/split line #" ")
        group-sizes (map read-string (s/split group-sizes-str #","))
        lazy-re (make-re-lazy-separators group-sizes)
        _ (prn "LAZY RE" lazy-re)
        greedy-re (make-re-greedy-separators group-sizes)
        _ (prn "GREEDY RE" greedy-re)
        lazy (query lazy-re pattern)
        _ (prn "LAZY" lazy)
        greedy (query greedy-re pattern)
        _ (prn "GREEDY" greedy)]
    (sets/union lazy greedy)))

(defn sum-of-possible-arrangements [input]
  (let [parsed-input (s/split-lines input)]
    (transduce
     (comp (map possible-arrangements)
           (map count))
     +
     parsed-input)))

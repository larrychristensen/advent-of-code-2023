(ns advent-of-code.day-13
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(defn is-reflection? [pattern-lines line-count i]
  (let [num-lines (min i (- line-count i))
        drop-count (- i num-lines)
        [lines-1 lines-2] (->> pattern-lines
                               (drop drop-count)
                               (partition num-lines))]
    (= lines-1 (reverse lines-2))))

(defn find-horizontal-reflection [pattern-lines]
  (let [line-count (count pattern-lines)]
    (into
     #{}
     (filter (partial is-reflection? pattern-lines line-count))
     (range 1 line-count))))

(defn transpose [pattern-lines]
  (map
   (fn [x]
     (map
      (fn [y]
        (get-in pattern-lines [y x]))
      (range (count pattern-lines))))
   (range (count (first pattern-lines)))))

(def find-vertical-reflection (comp find-horizontal-reflection transpose))

(defn reflection-value [vertical-reflections horizontal-reflections]
  (+ (apply + vertical-reflections)
     (* 100 (apply + horizontal-reflections))))

(defn pattern-reflection-value [pattern]
  (reflection-value (find-vertical-reflection pattern)
                    (find-horizontal-reflection pattern)))

(defn parse-pattern [pattern-str]
  (->> pattern-str s/split-lines (mapv vec)))

(defn part-1 [input]
  (let [patterns (s/split input #"\n\n")]
    (transduce
     (comp
      (map parse-pattern)
      (map pattern-reflection-value))
     +
     patterns)))

(defn new-reflection-value [pattern
                            original-horizontal
                            original-vertical
                            [y x]]
  (let [updated (update-in pattern
                           [y x]
                           #(if (= \. %) \# \.))
        vertical-reflection (find-vertical-reflection updated)
        horizontal-reflection (find-horizontal-reflection updated)
        horizontal-diff (sets/difference horizontal-reflection
                                         original-horizontal)
        vertical-diff (sets/difference vertical-reflection
                                       original-vertical)]
    (when (or (seq vertical-diff)
              (seq horizontal-diff))
      (reflection-value vertical-diff
                        horizontal-diff))))

(defn fixed-pattern-reflection-value [pattern]
  (let [original-horizontal (find-horizontal-reflection pattern)
        original-vertical (find-vertical-reflection pattern)]
    (some
     (partial new-reflection-value
              pattern
              original-horizontal
              original-vertical)
     (for [y (range (count pattern))
           x (range (count (first pattern)))]
       [y x]))))

(defn part-2 [input]
  (let [patterns (s/split input #"\n\n")]
    (transduce
     (comp
      (map parse-pattern)
      (map fixed-pattern-reflection-value))
     +
     patterns)))

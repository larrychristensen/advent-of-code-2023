(ns advent-of-code.day-9
  (:require [clojure.string :as s]))

(defn parse-lines [i]
  (map #(read-string (format "[%s]" %)) (s/split-lines i)))

(def diffs #(map - (rest %) %))

(def diff-stack #(->> % (iterate diffs) (take-while seq) reverse))

(defn sum-of-extrapolations [i e-fn]
  (->> (parse-lines i)
       (map #(reduce e-fn 0 (diff-stack %)))
       (reduce +)))

(defn pt1 [input]
  (sum-of-extrapolations input #(+ % (last %2))))

(defn pt2 [input]
  (sum-of-extrapolations input #(- (first %2) %)))


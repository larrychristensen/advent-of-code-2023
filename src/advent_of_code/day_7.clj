(ns advent-of-code.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

;; PART 1
(defn pt1 [i]
  (->> (re-seq #"(\w+) (\d+)" i)
       (sort-by
        (fn [[_ c]]
          [(->> c frequencies vals (map #(* % %)) (reduce +))
           (s/replace c #"\D" {"A" "Z" "K" "Y" "Q" "X" "J" "W" "T" "V"})]))
       (map-indexed #(-> %2 (nth 2) read-string (* (inc %))))
       (apply +)))

;; PART 2
(def u #(assoc % 0 (+ (% 0) %2)))

(def r #(-> % (dissoc \J) vals sort reverse vec (u (% \J 0))))

(defn pt2 [i]
  (->> (re-seq #"(\w+) (\d+)" i)
       (sort-by
        (fn [[_ c]]
          [(->> c frequencies r (map #(* % %)) (reduce +))
           (s/replace c #"\D" {"A" "Z" "K" "Y" "Q" "X" "J" "1" "T" "V"})]))
       (map-indexed #(-> %2 (nth 2) read-string (* (inc %))))
       (apply +)))



(ns advent-of-code.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-int [v]
  (Long/parseLong v))

(defn numbers-in-line [line]
  (map
   parse-int
   (re-seq #"\d+" line)))

(def numbers-in-lines-xform (map numbers-in-line))

(defn numbers-in-lines [lines-str]
  (sequence
   numbers-in-lines-xform
   (s/split-lines lines-str)))

(defn race-record [time distance]
  {:time time
   :distance distance})

(defn parse-race-records [input]
  (let [[times distances] (numbers-in-lines input)]
    (map
     race-record
     times
     distances)))

(defn time-to-distance [race-time hold-time]
  (* hold-time (- race-time hold-time)))

(defn hold-times-that-beat-record [race-time distance hold-times]
  (sequence
   (take-while
    (fn [hold-time]
      (> (time-to-distance race-time hold-time) distance)))
   hold-times))

(defn range-starts [time]
  (let [half-time (int (/ time 2))]
    [half-time (inc half-time)]))

(defn ways-to-beat-record [{:keys [time distance]}]
  (let [[start-dec start-inc] (range-starts time)
        forward-hold-times (range start-inc time)
        backward-hold-times (range start-dec 0 -1)]
    (apply
     concat
     (map
      (partial hold-times-that-beat-record time distance)
      [forward-hold-times backward-hold-times]))))

;; Part 1
(defn product-of-the-number-of-ways-to-beat-races [input]
  (transduce
   (comp (map ways-to-beat-record)
         (map count))
   *
   (parse-race-records input)))

;; Part 2
(defn parse-race-record [input]
  (let [[time distance] (sequence
                         (comp numbers-in-lines-xform
                               (map (partial apply str))
                               (map parse-int))
                         (s/split-lines input))]
    (race-record time distance)))

(defn number-of-ways-to-beat-race [input]
  (count
   (ways-to-beat-record
    (parse-race-record input))))

;; Code golf version
(defn g [i]
  (let [[t d] (map
               #(Long/parseLong %)
               (re-seq #"\d+" (s/replace i #"\s" "")))]
    (count
     (filter
      (fn [h]
        (< d (* h (- t h))))
      (range 1 t)))))

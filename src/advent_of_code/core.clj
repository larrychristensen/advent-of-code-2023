(ns advent-of-code.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn digits-in-word [word]
  (re-seq #"\d" word))

(def digit-spellings-to-values
  (array-map
   "one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9))

(defn standardize-digit [digit]
  (if-let [digit-value (digit-spellings-to-values digit)]
    (str digit-value)
    digit))

(def regex-str (format "(?=(%s|\\d))" (s/join "|" (keys digit-spellings-to-values))))

(def regex (re-pattern regex-str))

(defn first-digit [v]
  (first (re-find regex v)))

(defn digits-in-word-2 [word]
  (sequence
   (comp
    (map second)
    (map standardize-digit))
   (re-seq regex word)))

(def first-and-last-item (juxt first last))

(def concat-strs (partial apply str))

(defn parse-int [v]
  (Integer/parseInt v))

(defn sum-of-calibration-values-for-digits-fn [digits-fn filename]
  (with-open [reader (io/reader (io/resource filename))]
    (transduce
     (comp
      (map digits-fn)
      (map first-and-last-item)
      (map concat-strs)
      (map parse-int))
     +
     (line-seq reader))))

(def sum-of-calibration-values
  (partial sum-of-calibration-values-for-digits-fn digits-in-word))

(def sum-of-calibration-values-part-2
  (partial sum-of-calibration-values-for-digits-fn digits-in-word-2))

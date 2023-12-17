(ns advent-of-code.day-15
  (:require [clojure.string :as s]))

(defn hash-value [step-str]
  (reduce
   (fn [current c]
     (-> c int (+ current) (* 17) (rem 256)))
   0
   step-str))

(defn sum-of-hash-values [v]
  (transduce
   (map hash-value)
   +
   v))

(defn part-1 [input]
  (sum-of-hash-values (s/split input #",")))

(defn assemble-boxes [instructions]
  (reduce
   (fn [boxes instruction]
     (let [[_ label number] (re-matches #"(\w+)[=-](\d+)?" instruction)
           hash (hash-value label)]
       (if number
         (if (boxes hash)
           (assoc-in boxes [hash label] number)
           (assoc boxes hash (array-map label number)))
         (update boxes hash dissoc label))))
   {}
   instructions))

(defn focusing-power [[box-number box]]
  (transduce
   (map-indexed
    (fn [i [label focal-length]]
      (* (inc box-number) (inc i) (read-string focal-length))))
   +
   box))

(defn part-2 [input]
  (let [instructions (s/split input #",")]
    (transduce
     (map focusing-power)
     +
     (assemble-boxes instructions))))

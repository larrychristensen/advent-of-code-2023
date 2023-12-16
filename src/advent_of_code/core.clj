(ns advent-of-code.core
  (:require [clojure.string :as s]
            [clj-http.client :as client]))

(defn p-sect [input]
  (s/split input #"\n\n"))

(defn p-aaa [input]
  (map #(re-seq #"\w+" %) (s/split-lines input)))

(defn p-an [input]
  (mapv read-string (re-seq #"[-\d]+" input)))

(defn p-ana [input & [drop-row-count]]
  (mapv
   p-an
   (drop (or drop-row-count 0)
         (s/split-lines input))))

(defn p-htup [input]
  (apply map vector (p-ana input)))

(defn p-hmap [input]
  (into {} (p-htup input)))

(defn rot-l [crate-seq]
  (mapv
   (fn [i]
     (mapv
      (fn [j] (get-in crate-seq [j i]))
      (range (count crate-seq))))
   (range (count (first crate-seq)))))

(defn rot-r [v]
  (mapv
   (comp vec reverse)
   (rot-l v)))


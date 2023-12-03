(ns advent-of-code.day-3
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn re-seq-with-start-and-end [regex s]
  (let [matcher (re-matcher regex s)
            matches (atom [])]
    (while (.find matcher)
      (swap! matches conj {:start (.start matcher) :end (.end matcher) :group (.group matcher)}))
    @matches))

(defn add-line-indexes [i matches]
  (map
   (fn [match]
     (assoc match :line i))
   matches))

(defn add-type [match]
  (assoc
   match
   :type
   (if (re-matches #"\W" (:group match))
     :symbol
     :number)))

(defn schematic-values [lines]
  (sequence
    (comp
     (map (partial re-seq-with-start-and-end #"\d+|\.|\W"))
     (map-indexed add-line-indexes)
     cat
     (remove (fn [{:keys [group]}] (= group ".")))
     (map add-type))
    lines))

(defn parse-schematic [lines]
  (group-by
   :type
   (schematic-values lines)))

(defn adjacent? [number-record symbol-record]
  (and
   (<=
    (dec (:line number-record))
    (:line symbol-record)
    (inc (:line number-record)))
   (<=
    (dec (:start number-record))
    (:start symbol-record)
    (:end number-record))))

(defn part-number? [symbol-records number-record]
  (some
   (fn [symbol-record]
     (adjacent? number-record symbol-record))
   symbol-records))

(defn sum-of-part-numbers-for-lines [lines]
  (let [{:keys [number symbol]} (parse-schematic lines)]
      (transduce
       (comp
        (filter (partial part-number? symbol))
        (map :group)
        (map #(Integer/parseInt %)))
       +
       number)))

(defn adjacent-numbers [number-records symbol-record]
  (filter
   (fn [number-record]
     (adjacent? number-record symbol-record))
   number-records))

(defn gear-ratio [number-records]
  (transduce
   (comp
    (map :group)
    (map #(Integer/parseInt %)))
   *
   number-records))

(defn sum-of-gear-ratios-for-lines [lines]
  (let [{:keys [number symbol]} (parse-schematic lines)]
    (transduce
     (comp
      (map (partial adjacent-numbers number))
      (filter (comp (partial = 2) count))
      (map gear-ratio))
     +
     symbol)))

(defn sum-of-part-numbers [schematic-filename]
  (with-open [reader (io/reader (io/resource schematic-filename))]
    (sum-of-part-numbers-for-lines (line-seq reader))))

(defn sum-of-gear-ratios [schematic-filename]
  (with-open [reader (io/reader (io/resource schematic-filename))]
    (sum-of-gear-ratios-for-lines (line-seq reader))))

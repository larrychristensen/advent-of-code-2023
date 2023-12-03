(ns advent-of-code.day-4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.set :as sets]))

(defn parse-scratchcard-sections [scratchcard-line-str]
  (rest (re-matches #"Card\s+(\d+): (.+) \| (.+)" scratchcard-line-str)))

(defn parse-int [number-str]
  (Integer/parseInt number-str))

(defn parse-numbers-section [numbers-str]
  (into
   #{}
   (map parse-int)
   (re-seq #"\d+" numbers-str)))

(defn parse-scratchcard [scratchcard-line-str]
  (let [[card-number-str winning-numbers-str card-numbers-str]
        (parse-scratchcard-sections scratchcard-line-str)

        card-number (parse-int card-number-str)
        winning-numbers (parse-numbers-section winning-numbers-str)
        card-numbers (parse-numbers-section card-numbers-str)]
    {:number card-number
     :winning-numbers winning-numbers
     :actual-numbers card-numbers}))

(defn card-matches [scratchcard]
  (sets/intersection
   (:winning-numbers scratchcard)
   (:actual-numbers scratchcard)))

(defn card-match-count [scratchcard]
  (count (card-matches scratchcard)))

(defn scratchcard-value [scratchcard]
  (->> scratchcard
       card-match-count
       dec
       (Math/pow 2)
       int))

(defn scratchcard-line-value [scratchcard-line-str]
  (let [scratchcard (parse-scratchcard scratchcard-line-str)]
    (scratchcard-value scratchcard)))

(defn parse-scratchcards-file [scratchcards-table-filename]
  (with-open [reader (io/reader (io/resource scratchcards-table-filename))]
    (mapv
     parse-scratchcard
     (line-seq reader))))

(defn scratchcards-value [scratchcards-table-filename]
  (let [scratchcards (parse-scratchcards-file scratchcards-table-filename)]
    (transduce
     (map scratchcard-value)
     +
     scratchcards)))

(defn scratchcards-won-from-scratchcard [all-scratchcards scratchcard]
  (let [match-count (card-match-count scratchcard)]
    (sequence
     (comp (drop (:number scratchcard))
           (take match-count))
     all-scratchcards)))

(defn scratchcards-won-from-scratchcards
  [scratchcards-table
   unprocessed-scratchcards
   current-scratchcard
   processed-count]
  (if current-scratchcard
    (let [scratchcards-won (scratchcards-won-from-scratchcard
                            scratchcards-table
                            current-scratchcard)
          remaining-unprocessed-scratchcards (into
                                              unprocessed-scratchcards
                                              scratchcards-won)]
      (recur
       scratchcards-table
       (rest remaining-unprocessed-scratchcards)
       (first remaining-unprocessed-scratchcards)
       (inc processed-count)))
    processed-count))

(defn total-scratchcard-count [scratchcard-table-filename]
  (let [scratchcards (parse-scratchcards-file scratchcard-table-filename)]
    (scratchcards-won-from-scratchcards
     scratchcards
     (rest scratchcards)
     (first scratchcards)
     0)))

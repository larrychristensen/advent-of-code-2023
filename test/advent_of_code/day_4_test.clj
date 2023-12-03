(ns advent-of-code.day-4-test
  (:require
   [clojure.test :refer :all]
   [advent-of-code.day-4 :refer :all]))

(def example-line-str "Card  1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(deftest test-parse-scratchcard-sections
  (is (= ["1"
          "41 48 83 86 17"
          "83 86  6 31 17  9 48 53"]
         (parse-scratchcard-sections example-line-str))))

(deftest test-parse-numbers-section
  (let [numbers "15 16  23 42"]
    (is (= #{15 16 23 42} (parse-numbers-section numbers)))))

(deftest test-card-matches
  (is (= #{23 42}
         (card-matches
          {:winning-numbers #{4, 8, 15, 16, 23, 42}
           :actual-numbers #{67, 23, 42}}))))

(deftest test-scratchcard-line-value
  (is (= 8
         (scratchcard-line-value example-line-str))))

(deftest test-scratchcards-value
  (is (= 13
         (scratchcards-value "example-input-4.txt"))))

(deftest test-total-scratchcard-count
  (is (= 30
         (total-scratchcard-count "example-input-4.txt"))))

(deftest test-scratchcards-won-from-scratchcard
  (let [scratchcards (parse-scratchcards-file  "example-input-4.txt")
        scratchcard (first scratchcards)
        result (scratchcards-won-from-scratchcard scratchcards scratchcard)]
    (is (= [2 3 4 5] (map :number result)))))

(deftest test-scratchcards-won-from-scratchcards
  (let [scratchcards (parse-scratchcards-file "example-input-4.txt")
        scratchcard (first scratchcards)
        result (scratchcards-won-from-scratchcards
                scratchcards
                (rest scratchcards)
                (first scratchcards)
                0)]
    (is (= 30 result))))

(deftest test-total-scratchcard-count
  (is (= 30 (total-scratchcard-count "example-input-4.txt"))))

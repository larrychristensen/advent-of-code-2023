(ns advent-of-code.day-6-test
  (:require
   [clojure.test :refer :all]
   [advent-of-code.day-6 :refer :all]))

(def example-input
  "Time:      7  15   30
Distance:  9  40  200")

(deftest test-parse-race-records
  (is  (= [{:time 7
            :distance 9}
           {:time 15
            :distance 40}
           {:time  30
            :distance 200}]
          (parse-race-records example-input))))

(deftest test-ways-to-beat-record
  (is (= (sort
          [2 3 4 5])
         (sort
          (ways-to-beat-record {:time 7
                                :distance 9})))))

(deftest test-time-to-distance
  (is (= 12
         (time-to-distance 7 3)))
  (is (= 10
         (time-to-distance 7 5))))

(deftest test-range-starts
  (is (= [3 4] (range-starts 7)))
  (is (= [3 4] (range-starts 6))))

(deftest test-product-of-the-number-of-ways-to-beat-races
  (is (= 288
         (product-of-the-number-of-ways-to-beat-races
          example-input))))

(deftest test-parse-race-record
  (is (= {:time 71530
          :distance 940200}
         (parse-race-record example-input))))

(deftest test-number-of-ways-to-beat-race
  (is (= 71503
         (number-of-ways-to-beat-race
          example-input))))

(deftest test-golf
  (is (= 71503
         (g example-input))))


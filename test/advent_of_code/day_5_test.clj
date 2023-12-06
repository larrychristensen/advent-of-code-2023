(ns advent-of-code.day-5-test
  (:require
   [clojure.test :refer :all]
   [advent-of-code.day-5 :refer :all]))

(def input-filename "example-input-5.txt")

(def example-seeds
  "seeds: 79 14 55 13")

(def expected-parsed-input-2
  {:seeds [79 14 55 13]
   :maps [[[50 98 2]
           [52 50 48]]
          [[0 15 37]
           [37 52 2]
           [39 0 15]]
          [[49 53 8]
           [0 11 42]
           [42 0 7]
           [57 7 4]]
          [[88 18 7]
           [18 25 70]]
          [[45 77 23]
           [81 45 19]
           [68 64 13]]
          [[0 69 1]
           [1 0 69]]
          [[60 56 37]
           [56 93 4]]]})

(def expected-parsed-input
  {:seeds [79 14 55 13]
   :seed-to-soil [[50 98 2]
                  [52 50 48]]
   :soil-to-fertilizer [[0 15 37]
                        [37 52 2]
                        [39 0 15]]
   :fertilizer-to-water [[49 53 8]
                         [0 11 42]
                         [42 0 7]
                         [57 7 4]]
   :water-to-light [[88 18 7]
                    [18 25 70]]
   :light-to-temperature [[45 77 23]
                          [81 45 19]
                          [68 64 13]]
   :temperature-to-humidity [[0 69 1]
                             [1 0 69]]
   :humidity-to-location [[60 56 37]
                          [56 93 4]]})

(def example-input-map
  "seed-to-soil map:
50 98 2
52 50 48")

(def example-parsed-input-map
  [[50 98 2]
   [52 50 48]])

(def example-parsed-input-map-entry
  [:seed-to-soil example-parsed-input-map])

(deftest test-parse-seeds-to-locations
  (is (= expected-parsed-input-2
         (parse-seeds-to-locations-input-2 (get-input "example-input-5.txt")))))

(deftest test-make-lookup-function-for-input-map
  (let [lookup-fn (make-lookup-function-for-input-map
                   (:seed-to-soil expected-parsed-input))]
    (is (= 50 (lookup-fn 98)))
    (is (= 81 (lookup-fn 79)))
    (is (= 10 (lookup-fn 10)))
    (is (= 14 (lookup-fn 14)))))

(deftest test-parse-input-map
  (is (= example-parsed-input-map-entry
         (parse-input-map example-input-map))))

(deftest test-parse-seeds
  (is (= [79 14 55 13]
         (parse-seeds example-seeds))))

(def example-lookup-order
  [:seed-to-soil-map
   :soil-to-fertilizer-map
   :fertilizer-to-water-map])

(deftest test-seed-number-to-soil-number
  (let [seeds-to-locations (parse-seeds-to-locations-input
                            (get-input input-filename))]
    (let [seed-to-soil-map (:seed-to-soil-map seeds-to-locations)]
      (is (= 50 (seed-to-soil-map 98)))
      (is (= 81 (seed-to-soil-map 79))))))

(deftest test-lowest-location-number-for-seeds
  (is (= 35
         (lowest-location-number-for-seeds input-filename))))

(deftest test-header->name-map
  (is (= :temperature-to-humidity
         (header->map-name "temperature-to-humidity map"))))

(deftest test-seed-ranges-xform
  (is (= 27 (count
             (sequence
              seed-ranges-xform
              [79 14 55 13])))))

(deftest test-location-to-seed-map
  (is (= [[98 50 2]
          [50 52 48]]
         (:seed-to-soil (make-location-to-seed-map expected-parsed-input)))))

(deftest test-in-seeds
  (let [seeds [23 4]]
    (is (not (in-seeds? seeds 22)))
    (is (in-seeds? seeds 23))
    (is (in-seeds? seeds 24))
    (is (in-seeds? seeds 25))
    (is (in-seeds? seeds 26))
    (is (not (in-seeds? seeds 27)))))





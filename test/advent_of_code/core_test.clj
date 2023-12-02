(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.core :refer :all]))

(deftest test-example
  (testing "That it produces the correct value"
    (is (= 142 (sum-of-calibration-values "example-input-1.txt")))))

(deftest test-digits-in-word
  (is (= ["1" "3" "4"] (digits-in-word "a1w34b"))))

(deftest test-first-and-last-item
  (is (= [1 4]  (first-and-last-item [1 3 4])))
  (is (= [1 1] (first-and-last-item [1]))))

(deftest test-parse-int
  (is (= 23 (parse-int "23"))))

(deftest test-concat-strs
  (is (= "23" (concat-strs ["2" "3"]))))

(deftest test-sum-of-calibration-values-part-2
  (testing "That it produces the correct value"
    (is (= 281 (sum-of-calibration-values-part-2 "example-input-1-2.txt")))))

(deftest test-digits-in-word-2
  (is (= ["1"  "3" "3" "4" "4"] (digits-in-word-2 "aonex33uixfour4b")))
  (is (= ["9" "2" "9" "3" "7" "8" "8" "2"] (digits-in-word-2 "9sznchv2nineclrfdpxnf378xhkjbbplqdeightwos"))))

(deftest test-regex-str
  (is (= "(?=(one|two|three|four|five|six|seven|eight|nine|\\d))" regex-str)))

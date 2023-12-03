(ns advent-of-code.day-3-test
  (:require
   [clojure.test :refer :all]
   [advent-of-code.day-3 :refer :all]))

(deftest test-parse-schematic
  (let [schematic ["12.."
                                    "...$"
                                    "3..4"]]
    (is
     (=
      {:number
       [{:group "12"
         :type :number
         :line 0
         :start 0
         :end 2}
         {:type :number, :group "3", :line 2, :start 0, :end 1}
        {:group "4"
         :type :number
         :line 2
         :start 3
         :end 4}]
       :symbol
       [{:group "$"
          :type :symbol
          :line 1
          :start 3
          :end 4}]}
        (parse-schematic schematic)))))

(deftest test-sum-of-part-numbers
  (is
   (=
    4361
    (sum-of-part-numbers "example-input-3.txt"))))

(deftest test-re-seq-with-start-and-end
  (is
   (=
    [{:start 1 :end 2 :group "1"} {:start 5 :end 7 :group "23"}]
    (re-seq-with-start-and-end #"\d+" ".1...23...%"))))

(deftest test-adjacent?
  (is (adjacent? {:start 1 :end 3 :line 0} {:start 0 :end 1 :line 0}))
  (is (adjacent? {:start 1 :end 3 :line 0} {:start 3 :end 4 :line 0}))
  (is (adjacent? {:start 1 :end 3 :line 0} {:start 0 :end 1 :line 1}))
  (is (adjacent? {:start 1 :end 3 :line 1} {:start 0 :end 1 :line 0}))
  (is (adjacent? {:start 1 :end 3 :line 0} {:start 1 :end 2 :line 1})))

(deftest test-part-number?
  (is (part-number? [{:start 0 :end 1 :line 0}] {:start 1 :end 3 :line 0})))

(deftest test-sum-of-gear-ratios
  (is
   (=
    467835
    (sum-of-gear-ratios "example-input-3.txt"))))

(deftest test-gear-ratio
  (is
   (=
    16345
    (gear-ratio
     [{:group "467"}
       {:group "35"}]))))

(ns advent-of-code.day-2-test
  (:require
   [clojure.test :refer :all]
   [advent-of-code.day-2 :refer :all]))

(def example-trials
  [{"blue" 3
    "red" 4}
   {"red" 1
    "green" 2
    "blue" 6}
   {"green" 2}])

(def example-game
  {:id 1
   :trials example-trials})

(def example-game-trials-str
  "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(deftest test-possible-game-id-sum
  (is (= 8
         (sum-of-the-ids-of-possible-games
          {"red" 12
           "green" 13
           "blue" 14}
          "example-input-2.txt"))))

(deftest test-parse-game-record
  (is (=
       example-game
       (parse-game-record (format "Game 1: %s" example-game-trials-str)))))

(deftest test-parse-trial-count
  (is (= ["blue" 2] (parse-trial-count "2 blue"))))

(deftest test-parse-trial-record
  (is (= {"blue" 2 "green" 5}
             (parse-trial-record "2 blue, 5 green"))))

(deftest test-parse-trial-records
  (is (=
       example-trials
       (parse-trial-records example-game-trials-str))))

(deftest test-sum-of-the-powers-of-minimum-sets
  (is (=
       2286
       (sum-of-the-powers-of-minimum-sets "example-input-2.txt"))))

(deftest test-game-minimum-set
  (is (=
       {"blue" 6 "red" 4 "green" 2}
       (game-minimum-set example-game))))

(deftest test-power-of-minimum-set
  (is
   (=
    48
    (power-of-minimum-set
     {"red" 2
     "green" 4
     "blue" 6}))))

(ns advent-of-code.day-14
  (:require [clojure.string :as s])
  (:import [java.security MessageDigest]
           [java.util Base64]))

(defn transpose
  "Linear algebra transposition, i.e. flips on the diagonal so that
  [i, j] -> [j, i]"
  [lines]
  (apply map vector lines))

(def sort-desc (partial sort #(compare %2 %)))

(defn tilt-west-east [sort-fn platform]
  (mapv
   ;; divide the empty spaces between cube rocks, sort the
   ;; the round rocks to the east or west, and join back
   ;; together.
   #(->> % (partition-by #{\#}) (into [] (mapcat sort-fn)))
   platform))

(def tilt-west (partial tilt-west-east sort-desc))

;; this allows reusing east-west tilt logic rather than having
;; to implement another low-level tilt function
(def tilt-north-south #(-> %2 transpose %1 transpose))

(def tilt-north (partial tilt-north-south tilt-west))

(defn total-load [parsed]
  (let [line-count (count parsed)]
    (->> parsed
         (map-indexed
          #(->> %2 (filter #{\O}) count (* (- line-count %1))))
         (reduce +))))

(def parse-platform #(map vec (s/split-lines %)))

(def part-1 #(->> % parse-platform tilt-north total-load))


;; PART 2

(def tilt-east (partial tilt-west-east sort))

(def tilt-south (partial tilt-north-south tilt-east))

(def tilt-cycle #(-> % tilt-north tilt-west tilt-south tilt-east))

(def tilt-cycle-mem (memoize tilt-cycle))

(defn platform-hash
  "Converts a platform into a SHA-256 in Base64, which allows
  for checking cycles without using a ton of memory"
  [platform]
  (->> platform
       flatten
       (apply str)
       (.getBytes)
       (.digest (MessageDigest/getInstance "SHA-256"))
       (.encodeToString (Base64/getEncoder))))

(defn cycle-end-and-start
  "Returns a pair containing the second and first cycle nums that
  return the same value"
  [platform]
  (loop [current-platform platform
         num-cycles 0
         hashes {}]
    (let [hash (platform-hash current-platform)
          updated-hashes (update hashes hash conj num-cycles)
          hash-cycles (hashes hash)]
      (if (= (count hash-cycles) 2)
        hash-cycles
        (recur (tilt-cycle current-platform)
               (inc num-cycles)
               updated-hashes)))))

(defn part-2
  "The key thing here is to recognize that 1B cycles will take a
  long time. Since AoC says every solution should take less than a
  few minutes, there must be a shortcut. I suspected that this
  meant that there was a steady-state point where the positions
  cycle. Thus the solution is to just find the result after
  `cycle-start` + ((1e9 - `cycle-start`) mod `cycle-length`)
  iterations."
  [input]
  (let [platform (parse-platform input)
        [end start :as c] (cycle-end-and-start platform)
        remaining (rem (- 1e9 start) (apply - c))
        num-iterations (+ start remaining)]
    (->> (iterate tilt-cycle platform)
         (drop num-iterations)
         first
         total-load)))

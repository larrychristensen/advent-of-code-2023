(ns advent-of-code.day-2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn parse-trial-count [trial-count-string]
  (let [[_ count-str color-str] (re-matches #"(\d+) (.+)" trial-count-string)]
    [color-str (Integer/parseInt count-str)]))

(defn parse-trial-record [trial-record-string]
  (into
   {}
   (map
    parse-trial-count)
   (s/split trial-record-string #", ")))

(defn parse-trial-records [trial-records-string]
  (map
   parse-trial-record
   (s/split trial-records-string #"; ")))

(defn parse-game-record [game-record-string]
  (let [[_ id-str trial-records-string] (re-matches #"Game (\d+): (.+)" game-record-string)
        id (Integer/parseInt id-str)]
    {:id id
     :trials (parse-trial-records trial-records-string)}))

(defn possible-game? [color-to-count-map game-record]
  (every?
   (fn [trial]
     (every?
      (fn [[color-str color-count]]
        (<= color-count (color-to-count-map color-str)))
      trial))
   (:trials game-record)))

(defn sum-of-the-ids-of-possible-games [color-to-count-map filename]
  (with-open [reader (io/reader (io/resource filename))]
    (transduce
     (comp
      (map parse-game-record)
      (filter (partial possible-game? color-to-count-map))
      (map :id))
     +
     (line-seq reader))))

(defn game-minimum-set [game-record]
  (apply merge-with max (:trials game-record)))

(defn power-of-minimum-set [minimum-set]
  (apply * (vals minimum-set)))

(defn sum-of-the-powers-of-minimum-sets [filename]
  (with-open [reader (io/reader (io/resource filename))]
    (transduce
     (comp
      (map parse-game-record)
      (map game-minimum-set)
      (map power-of-minimum-set))
     +
     (line-seq reader))))

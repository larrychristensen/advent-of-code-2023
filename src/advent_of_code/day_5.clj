(ns advent-of-code.day-5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.set :as sets]))

(def lookup-order
  [:seed-to-soil
   :soil-to-fertilizer
   :fertilizer-to-water
   :water-to-light
   :light-to-temperature
   :temperature-to-humidity
   :humidity-to-location])

(defn parse-int [v]
  (Long/parseLong v))

(defn parse-numbers-from-line [line-str]
  (mapv
   parse-int
   (re-seq #"\d+" line-str)))

(defn parse-seeds [seeds-str]
  (parse-numbers-from-line seeds-str))

(defn header->map-name [header]
  (let [[map-name] (s/split header #" ")]
    (keyword map-name)))

(defn parse-input-map [map-str]
  (let [[header & lines] (s/split map-str #"\n")
        map-name (header->map-name map-str)]
    [map-name
     (map
      parse-numbers-from-line
      lines)]))

(defn parse-input-map-2 [map-str]
  (let [[header & lines] (s/split map-str #"\n")
        map-name (header->map-name map-str)]
    (mapv
     parse-numbers-from-line
     lines)))

(defn lookup-destination-number-in-map-line [item-number input-map-line]
  (let [[dest-start src-start num] input-map-line]
    (when (>= item-number src-start)
      (let [diff (- item-number src-start)]
        (when (<= diff (dec num))
          (+ dest-start diff))))))

(defn lookup-destination-number-in-map [input-map item-number]
  (some
   (partial lookup-destination-number-in-map-line item-number)
   input-map))

(defn make-lookup-function-for-input-map [input-map]
  (fn [item-number]
    (or
     (lookup-destination-number-in-map input-map item-number)
     item-number)))

(defn parse-seeds-to-locations-input [input]
  (let [[seeds-str & maps-strs] (s/split input #"\n\n")]
    (into
     {:seeds (parse-seeds seeds-str)}
     (map parse-input-map)
     maps-strs)))

(defn parse-seeds-to-locations-input-2 [input]
  (let [[seeds-str & maps-strs] (s/split input #"\n\n")]
    {:seeds (parse-seeds seeds-str)
     :maps (mapv
            parse-input-map-2
            maps-strs)}))

(defn get-input [input-filename]
  (slurp (io/resource input-filename)))

(defn make-lookup-chain [seeds-to-locations
                         lookup-order]
  (let [lookup-functions (into
                          {}
                          (map
                           (fn [[k v]]
                             [k (make-lookup-function-for-input-map v)]))
                          (dissoc seeds-to-locations :seeds))
        {:keys [seed-to-soil
                soil-to-fertilizer
                fertilizer-to-water
                water-to-light
                light-to-temperature
                temperature-to-humidity
                humidity-to-location]} lookup-functions]
    (fn [v]
      (-> v
          seed-to-soil
          soil-to-fertilizer
          fertilizer-to-water
          water-to-light
          light-to-temperature
          temperature-to-humidity
          humidity-to-location)))
  #_(apply
   comp
   (reverse
    (map
     (fn [map-key]
       (-> map-key
           seeds-to-locations
           make-lookup-function-for-input-map))
     lookup-order))))

(defn lowest-location-number-for-seeds-with-seed-xform
  [seed-xform input-filename]
  (let [input (get-input input-filename)
        seeds-to-locations (parse-seeds-to-locations-input input)
        lookup-fn (make-lookup-chain seeds-to-locations
                                     lookup-order)]
    (transduce
     (comp seed-xform
           (map lookup-fn))
     min
     Long/MAX_VALUE
     (:seeds seeds-to-locations))))

(defn make-seed-range [[start-number num]]
  (range start-number
         (+ start-number num)))

(def seed-ranges-xform
  (comp (partition-all 2)
        (mapcat make-seed-range)))

(defn lowest-location-number-for-seeds [input-filename]
  (lowest-location-number-for-seeds-with-seed-xform
   (map identity)
   input-filename))

(defn in-seeds? [seeds seed-number]
  (some
   (fn [[start num]]
     (<= start seed-number (+ start (dec num))))
   (partition-all 2 seeds)))

(defn make-location-to-seed-map [seeds-to-locations]
  (into
   {}
   (map
    (fn [[map-key item-map]]
      [map-key
       (map
        (fn [[dest src num]]
          [src dest num])
        item-map)]))
   (dissoc seeds-to-locations :seeds)))

(defn lowest-location-number-for-seeds-part-2 [input-filename]
  (lowest-location-number-for-seeds-with-seed-xform
   seed-ranges-xform
   input-filename))

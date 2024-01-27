(ns day03.core
  (:gen-class)
  (:require [clojure.set :as set]))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses an input line into a vector of 5 numbers."
  [line]
  (->> line
       (re-seq #"\d+|\d+,\d+|\d+x\d+")
       (mapv #(Integer/parseInt %))))

(defn parse-file
  "Reads and parses the input file into a vector of claims.
  Each claim is represented by a vector of 5 numbers
  (id, left offset, top offset, width, height)."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (mapv parse-line)))

(def memoized-input-file->claims (memoize parse-file))

(defn find-contained-squares
  "Returns a list of all squares contained in the given rectangle."
  [[left top width height]]
  (for [x (range left (+ left width))
        y (range top (+ top height))]
    [x y]))

(defn find-claimed-squares-frequency
  "Returns a map of the frequency count of all claimed squares."
  []
  (let [rectangles (map rest (memoized-input-file->claims))]
    (frequencies (reduce (fn [result rectangle]
                           (into result (find-contained-squares rectangle)))
                         []
                         rectangles))))

(def memoized-claimed-squares-frequency (memoize find-claimed-squares-frequency))

; --------------------------
; problem 1

(defn count-twice-claimed-squares
  "Returns the number of squares that have been claimed at least twice."
  []
  (->> (memoized-claimed-squares-frequency)
       vals
       (filter #(> % 1))
       count))

; --------------------------
; problem 2

(defn find-once-claimed-squares
  "Returns a vector of the squares that have been claimed only once."
  []
  (let [claimed-squares-frequencies (memoized-claimed-squares-frequency)]
    (reduce (fn [result [square freq]]
              (if (= freq 1)
                (conj result square)
                result))
            []
            claimed-squares-frequencies)))

(defn find-non-overlapping-claim
  "Find the id of the claim that does not overlap with any other claim."
  []
  (let [once-claimed-squares (set (find-once-claimed-squares))
        claims (memoized-input-file->claims)]
    (loop [[[id & rectangle] & rest-claims] claims]
      (when id
        (let [contained-squares (set (find-contained-squares rectangle))]
          (if (empty? (set/difference contained-squares once-claimed-squares))
            id
            (recur rest-claims)))))))

; --------------------------
; results

(defn day03-1
  []
  (count-twice-claimed-squares))

(defn day03-2
  []
  (find-non-overlapping-claim))

(defn -main
  []
  (println (day03-1))
  (println (day03-2)))

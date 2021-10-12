(ns day03.core
  (:gen-class)
  (:require [clojure.set :as set]))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses a string representing a single claim."
  [line]
  (let [no-delims (clojure.string/split line #" @ |,|: |x")]
    (mapv #(Integer/parseInt %) (rest no-delims))))

(defn parse
  "Splits the input string by \n and converts it to an appropriate data
  structure."
  [s]
  (mapv parse-line (clojure.string/split-lines s)))

(def claims (parse (slurp input-file)))

; --------------------------
; problem 1

(defn find-max-coordinate
  "Finds the max claimed coordinate."
  []
  (reduce (fn [[max-x max-y] [left top width height]]
            [(max max-x (+ left width))
             (max max-y (+ top height))])
          [0 0] claims))

(defn find-claimed
  "Returns a list of all claimed coordinates of a claim."
  [[left top width height :as claim]]
  (for [x (range left (+ left width))
        y (range top (+ top height))]
    [x y]))

(defn claimed?
  "Returns true if [x y] is a claimed coordinate."
  [[x y] [left top width height :as claim]]
  (and (<= left x (+ left width))
       (<= top y (+ top height))))

(defn find-multiple-claimed
  "Returns a vector of two set. The first set are the claimed coordinates which
  are claimed at least twice. The second set are the claimed coordinates which
  are claimed only one time."
  []
  (reduce (fn [[multiple-claimed not-claimed] claim]
            (let [claimed (set (find-claimed claim))
                  already-claimed (set/intersection multiple-claimed claimed)
                  new-claimed (set/intersection not-claimed claimed)
                  new-not-claimed (set/difference claimed already-claimed new-claimed)
                  old-not-claimed (set/difference not-claimed already-claimed new-claimed)
                  updated-multiple-claimed (into multiple-claimed new-claimed)
                  updated-not-claimed (into old-not-claimed new-not-claimed)]
              [updated-multiple-claimed updated-not-claimed]))
          [#{} #{}] claims))

(def memoized-find-multiple-claimed (memoize find-multiple-claimed))

; --------------------------
; problem 2

(defn find-non-overlapping-claim
  "Find the claim id that does not overlap with any other claim."
  []
  (let [single-claimed (second (memoized-find-multiple-claimed))]
    (loop [[claim & rest-claims] claims
           index 0]
      (if claim
        (let [claimed (set (find-claimed claim))]
          (if (empty? (set/difference claimed single-claimed))
            (inc index)
            (recur rest-claims (inc index))))))))

; --------------------------
; results

(defn day03-1
  []
  (count (first (memoized-find-multiple-claimed))))

(defn day03-2
  []
  (find-non-overlapping-claim))

(defn -main
  []
  (println (day03-1))
  (println (day03-2)))

(ns day03.core
  (:gen-class))

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

(defn -main
  []
  (println (find-max-coordinate)))

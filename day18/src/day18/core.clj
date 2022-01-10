(ns day18.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and returns the lumber area represented as a vector of vectors.
  Each vector represents a line in the input file.
  The contents of [i j] can be retrieved by get-in."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        rows (count split-lines)
        columns (count (first split-lines))]
    {:acres (mapv vec split-lines)
     :rows rows
     :columns columns}))

(def parsed-input (parse (slurp input-file)))
(def acres (:acres parsed-input))
(def rows (:rows parsed-input))
(def columns (:columns parsed-input))

(defn get-adjacent
  "Returns the values of adjacent locations to [x y]."
  [acres [x y]]
  (let [top-left (get-in acres [(dec x) (dec y)])
        top (get-in acres [(dec x) y])
        top-right (get-in acres [(dec x) (inc y)])
        left (get-in acres [x (dec y)])
        right (get-in acres [x (inc y)])
        bottom-left (get-in acres [(inc x) (dec y)])
        bottom (get-in acres [(inc x) y])
        bottom-right (get-in acres [(inc x) (inc y)])]
    (filter (complement nil?)
            [top-left top top-right left right bottom-left bottom bottom-right])))

(defn create-grid
  "Returns a collection of all [x y] locations."
  []
  (for [x (range rows)
        y (range columns)]
    [x y]))

(defn next-state
  "Returns the next state of an acre at loc."
  [acres loc]
  (let [loc-acre (get-in acres loc)
        adjacent-acres (get-adjacent acres loc)
        adjacent-freq (frequencies adjacent-acres)]
    (case loc-acre
      \. (if (>= (get adjacent-freq \| 0) 3)
           \|
           \.)
      \| (if (>= (get adjacent-freq \# 0) 3)
           \#
           \|)
      \# (if (and (>= (get adjacent-freq \# 0) 1)
                  (>= (get adjacent-freq \| 0) 1))
           \#
           \.))))

(defn -main
  []
  (println (next-state acres [0 0])))

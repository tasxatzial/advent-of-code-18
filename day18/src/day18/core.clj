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
  (let [split-lines (clojure.string/split-lines s)]
    (mapv vec split-lines)))

(def acres (parse (slurp input-file)))

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

(defn -main
  []
  (println (get-adjacent acres [0 0])))

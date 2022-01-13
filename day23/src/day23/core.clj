(ns day23.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses an input line (string) and returns a data structure that represents
  a nanobot: a vector of two items, position (vector) & radius."
  [s]
  (let [split-line (clojure.string/split s #"pos=<|>, r=")
        pos (read-string (str \[ (second split-line) \]))
        radius (Integer/parseInt (get split-line 2))]
    [pos radius]))

(defn parse
  "Parses the input string and returns a collection of nanobots.
  A nanobot is represented by the data structure returned by parse-line."
  [s]
  (let [split-lines (clojure.string/split-lines s)]
    (map parse-line split-lines)))

(def nanobots (parse (slurp input-file)))

(defn -main
  []
  (println nanobots))

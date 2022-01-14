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

; --------------------------
; problem 1

(defn distance
  "Returns the manhattan distance between pos1 and pos2."
  [pos1 pos2]
  (->> (map #(- %1 %2) pos1 pos2)
       (map #(Math/abs %))
       (apply +)))

(defn count-within-strongest
  "Counts how many nanobots are within the range of the strongest nanobot."
  []
  (let [max-radius (apply max (map second nanobots))
        [strongest-pos strongest-radius] (some #(when (= max-radius (second %)) %) nanobots)]
    (->> nanobots
         (map first)
         (map #(distance strongest-pos %))
         (filter #(<= % strongest-radius))
         count)))

; --------------------------
; results

(defn day23-1
  []
  (count-within-strongest))

(defn -main
  []
  (println (day23-1)))

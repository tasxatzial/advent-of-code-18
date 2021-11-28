(ns day12.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")
(def init-state "##.#.####..#####..#.....##....#.#######..#.#...........#......##...##.#...####..##.#..##.....#..####")

(defn parse-rule
  "Parses an input line that represents a rule to the appropriate structure."
  [rule]
  (let [split-line (clojure.string/split rule #" => ")
        match-pattern (into [] (first split-line))
        next-state (first (seq (second split-line)))]
    [match-pattern next-state]))

(defn parse-rules
  "Parses the input string to the appropriate data structure (a map
  of rules). Each rule has as key a vector that describes the match pattern
  and as value a char that indicates the next state."
  [s]
  (let [data (clojure.string/split-lines s)
        rules (drop 2 data)]
    (into {} (map parse-rule rules))))

(def rules (parse-rules (slurp input-file)))

(defn -main
  []
  (println rules))

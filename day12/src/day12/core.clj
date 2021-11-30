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

; --------------------------
; problem 1

(defn get-last
  "Returns the nth element of coll (counting from the end)"
  [coll n]
  (get coll (- (count coll) n)))

(defn new-state-prefix
  "Returns a vector containing the number of \. that are necessary to be added
  as a prefix to the given state."
  [state]
  (cond
    (= \# (get state 2)) [\. \.]
    (= \# (get state 3)) [\.]
    :else []))

(defn new-state-suffix
  "Returns a vector containing the number of \. that are necessary to be added
  as a suffix to the given state."
  [state]
  (cond
    (= \# (get-last state 3)) [\. \.]
    (= \# (get-last state 4)) [\.]
    :else []))

(defn -main
  []
  (println rules))

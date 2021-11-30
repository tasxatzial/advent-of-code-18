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

(defn augment-state
  "Expands the stat vector with the prefix and suffix vectors."
  [state prefix suffix]
  (if (seq suffix)
    (if (seq prefix)
      (into (into prefix state) suffix)
      (into state suffix))
    state))

(defn trim-state
  "Removes any unnecessary \. from the start or end of the state.
  prefix-count and suffix-count count the number of \. that have already been
  added to the state as a prefix and suffix in the current iteration.
  Returns a vector containing the updated state and the adjusted prefix-count."
  [state prefix-count suffix-count]
  (if (and (zero? prefix-count) (= \. (get state 4)))
    (if (and (zero? suffix-count) (= \. (get-last state 5)))
      [(dec prefix-count) (subvec state 1 (- (count state) 2))]
      [(dec prefix-count) (subvec state 1)])
    [prefix-count state]))

(defn next-state
  "Finds the next state of the given state. Returns a vector containing:
  1) how many empty pots have been added to the start of the state
  2) the new state"
  [state]
  (let [prefix (new-state-prefix state)
        suffix (new-state-suffix state)
        augmented-state (augment-state state prefix suffix)
        [new-prefix-count ready-state] (trim-state augmented-state (count prefix) (count suffix))
        state-patterns (mapv #(subvec ready-state (- % 2) (+ % 3)) (range 2 (- (count ready-state) 2)))
        next-state (mapv #(get rules %) state-patterns)]
    [new-prefix-count (conj (into [\. \.] next-state) \. \.)]))

(defn evolve
  "Advances the state by the given number of generations. Returns a vector containing:
  1) how many empty pots have been added to the start of the final state
  2) the final state"
  ([state generations]
   (let [initial-state (conj (into [\. \. \. \.] state) \. \. \. \.)]
     (evolve initial-state generations 4)))
  ([state generations total-prefix]
   (if (zero? generations)
     [total-prefix state]
     (let [[prefix-count new-state] (next-state state)]
       (recur new-state (dec generations) (+ total-prefix prefix-count))))))

(defn -main
  []
  (println rules))

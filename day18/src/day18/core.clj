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
  "Returns the values of adjacent acres to the acre at location [x y]."
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
  "Returns a collection of all acre locations [x y]."
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

(def grid (create-grid))

(defn advance
  "Returns the next state of all acres."
  [acres]
  (->> grid
       (map #(next-state acres %))
       (partition columns)
       (mapv vec)))

(defn get-acre-at-N
  "Returns the acres at position N in the acres-seq assuming the
  acres at position i and (i - 1)/2 are the same."
  [acres-seq N i]
  (let [diff (inc (/ (dec i) 2))
        q (Math/round (Math/floor (/ (- N i) diff)))
        fi (+ (/ (dec i) 2) (- N (+ i (* q diff))))]
    (get acres-seq fi)))

(defn simulate
  "Runs the simulation for the given number of steps and returns the
  lumber area after the given number of steps.
  Uses Floyd's cycle-finding algorithm."
  ([steps]
   (simulate steps [acres] 1))
  ([steps acres-seq i]
   (let [curr-acres (last acres-seq)]
     (if (= (inc steps) (count acres-seq))
       curr-acres
       (let [next-acres (advance curr-acres)
             new-acres (conj acres-seq next-acres)
             checked-acres (get new-acres (/ (dec i) 2))]
         (if (and (odd? i) (= next-acres checked-acres))
           (get-acre-at-N new-acres steps i)
           (recur steps new-acres (inc i))))))))

(defn day18
  [steps]
  (let [final-acres-freq (frequencies (flatten (simulate steps)))]
    (* (get final-acres-freq \#) (get final-acres-freq \|))))

(defn -main
  []
  (println (day18 10))
  (println (day18 1000000000)))

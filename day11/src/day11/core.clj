(ns day11.core
  (:gen-class))

(def grid-serial 6392)
(def max-subgrid-size 300)

; --------------------------
; problem 1

(defn find-cell-power
  "Calculates the power of the [x y] cell."
  [[x y]]
  (let [rack-id (+ x 10)
        power (* rack-id (+ (* y rack-id) grid-serial))]
    (if (>= power 100)
      (- (int (/ power 100)) (* 10 (int (/ power 1000))) 5)
      -5)))

(defn create-grid
  "Creates a vector that contains all [x y] coordinates with 1<=x<=max, 1<=y<=max"
  [max]
  (vec (for [x (range 1 (inc max))
             y (range 1 (inc max))]
         [y x])))

(defn find-subgrid-offsets
  "Returns a vector with the relative cell offsets to the top-left cell of a subgrid.
  The offsets indicate positions in the 300x300 grid."
  [subgrid-size]
  (let [x-offsets (range subgrid-size)
        y-offsets (range 0 (* 300 subgrid-size) 300)]
    (reduce (fn [result offset]
              (into result (map #(+ offset %) x-offsets)))
            [] y-offsets)))

(defn find-subgrid-power
  "Finds the total power of a subgrid."
  [cells-power topleft-index subgrid-size]
  (let [subgrid-offsets (find-subgrid-offsets subgrid-size)
        subgrid-powers (map #(get cells-power (+ topleft-index %)) subgrid-offsets)]
    (apply + subgrid-powers)))

(defn find-max-power
  "Finds the subgrid of the given size that has the most power. Returns a vector
  containing the power and the top-left [x y] coordinate of the subgrid."
  [subgrid-size]
  (let [cells-power (mapv find-cell-power (create-grid 300))
        scan-grid (create-grid (- 300 (dec subgrid-size)))]
    (reduce (fn [result [x y :as cell]]
              (let [subgrid-index (+ (dec x) (* (dec y) 300))
                    subgrid-power (find-subgrid-power cells-power subgrid-index subgrid-size)
                    max-power (first result)]
                (if (> subgrid-power max-power)
                  [subgrid-power cell]
                  result)))
            [0 0] scan-grid)))

; --------------------------
; problem 2

(defn sum-lines
  "Takes as input a collection of vectors of equal count. Returns a
  vector in which every item is the sum of all vectors up to and including
  that item."
  [coll]
  (reduce (fn [result line]
            (conj result (mapv + (last result) line)))
          [(into [] (first coll))] (rest coll)))

(defn select-nth
  "Takes as input a collection of vectors and returns a new vector that
  contains the nth elements of each vector."
  [n coll]
  (mapv #(get % n) coll))

(defn transpose
  "Returns the transpose of a matrix stored as a vector of row or column vectors."
  [coll]
  (mapv #(select-nth % coll) (range 0 max-subgrid-size)))

(defn create-summed-table
  "Returns the summed-area table of the vector that contains the
  power of each [x y] location. The table is stored as a vector
  of vectors, each vector corresponds to a grid row."
  [cell-powers]
  (let [by-row-partition (partition max-subgrid-size cell-powers)
        partial-sum1 (sum-lines by-row-partition)
        by-col-partition (transpose partial-sum1)
        partial-sum2 (sum-lines by-col-partition)]
    (transpose partial-sum2)))

; --------------------------
; results

(defn day11-1
  []
  (second (find-max-power 3)))

(defn -main
  []
  (println (day11-1)))

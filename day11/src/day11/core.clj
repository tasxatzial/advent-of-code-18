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
    (reduce into (transpose partial-sum2))))

(defn get-index
  "Returns the corresponding index of [x y] in a one dimensional vector."
  [x y]
  (+ x (* y max-subgrid-size)))

(defn get-top-left-sum
  "Finds the top-left sum needed by the summed-area algorithm for the
  subgrid with top-left corner at [x y]."
  [x y summed-table]
  (if (or (<= (dec x) 0) (<= (dec y) 0))
    0
    (get summed-table (get-index (dec x) (dec y)))))

(defn get-bottom-right-sum
  "Finds the bottom-right sum needed by the summed-area algorithm for the
  subgrid with top-left corner at [x y]."
  [x y summed-table size]
  (get summed-table (get-index (dec (+ x size)) (dec (+ y size)))))

(defn get-bottom-left-sum
  "Finds the bottom-left sum needed needed by the summed-area algorithm for
  the subgrid with top-left corner at [x y]."
  [x y summed-table size]
  (if (<= (dec x) 0)
    0
    (get summed-table (get-index (dec x) (+ (dec size) y)))))

(defn get-top-right-sum
  "Finds the top-right sum needed needed by the summed-area algorithm for
  the subgrid with top-left corner at [x y]."
  [x y summed-table size]
  (if (<= (dec y) 0)
    0
    (get summed-table (get-index (dec (+ x size)) (dec y)))))

(defn find-subgrid-power2
  "Calculates the sum of all values in the square subgrid of the given
  size and top-left corner at [x y]."
  [x y summed-table subgrid-size]
  (let [top-left-sum (get-top-left-sum x y summed-table)
        bottom-left-sum (get-bottom-left-sum x y summed-table subgrid-size)
        bottom-right-sum (get-bottom-right-sum x y summed-table subgrid-size)
        top-right-sum (get-top-right-sum x y summed-table subgrid-size)]
    (+ top-left-sum (- bottom-right-sum top-right-sum bottom-left-sum))))

(defn find-max-power2
  "Finds the max power among all subgrids of a given size.
  Returns a vector containing the power and the top left corner of the
  subgrid."
  [size summed-table grid]
  (let [init-power (find-subgrid-power2 0 0 summed-table size)]
    (reduce (fn [[max-power _ :as result] [x y :as pos]]
              (if (or (> (+ (dec x) size) max-subgrid-size)
                      (> (+ (dec y) size) max-subgrid-size))
                result
                (let [power (find-subgrid-power2 (dec x) (dec y) summed-table size)]
                  (if (> power max-power)
                    [power pos]
                    result))))
            [init-power [1 1]] (rest grid))))

; --------------------------
; results

(defn day11-1
  []
  (second (find-max-power 3)))

(defn -main
  []
  (println (day11-1)))

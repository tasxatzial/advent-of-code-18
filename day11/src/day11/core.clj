(ns day11.core
  (:gen-class))

(def grid-serial 6392)

(defn find-cell-power
  "Calculates the power of the [x y] cell."
  [[x y]]
  (let [rack-id (+ x 10)
        power (* rack-id (+ (* y rack-id) grid-serial))]
    (if (>= power 100)
      (- (int (/ power 100)) (* 10 (int (/ power 1000))) 5)
      -5)))

(defn create-grid
  "Creates a vector that contains all [x y] coordinates
   with 1<=x<=max, 1<=y<=max"
  [max]
  (vec (for [x (range 1 (inc max))
             y (range 1 (inc max))]
         [y x])) )

(def subgrid-offsets [0 1 2 300 301 302 600 601 602])

(defn find-subgrid-power
  "Finds the total power of a 3x3 subgrid."
  [cells-power topleft-index]
  (let [subgrid-powers (map #(get cells-power (+ topleft-index %)) subgrid-offsets)]
    (apply + subgrid-powers)))

(defn -main
  []
  (println (find-cell-power [3 5])))

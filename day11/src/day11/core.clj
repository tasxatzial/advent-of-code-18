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

(defn -main
  []
  (println (find-cell-power [3 5])))

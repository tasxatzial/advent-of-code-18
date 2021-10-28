(ns day06.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and converts it to a vector of vectors.
  Each vector represents a (x,y) coordinate."
  [s]
  (->> s
       (clojure.string/split-lines)
       (map #(clojure.string/split % #", "))
       (mapv #(vector (Integer/parseInt (first %))
                      (Integer/parseInt (second %))))))

(def coordinates (parse (slurp input-file)))

(defn manhattan-dist
  "Returns the manhattan distance of (x1,y2) and (x2,y2)."
  [[x1 y1] [x2 y2]]
  (+ (Math/abs ^int (- x1 x2))
     (Math/abs ^int (- y1 y2))))

(defn- find-extreme-point
  "Finds the {min,max} {x,y} coordinates.
  fn-extreme must be 'min' or 'max'.
  fn-coordinate must be 'first' (for the x coordinates) or 'second' (for the y coordinates)."
  [fn-extreme fn-coordinate]
  (->> coordinates
       (mapv fn-coordinate)
       (apply fn-extreme)))

(defn find-extreme-points
  "Finds the min x, max x, min y, max y input coordinates and returns
  them in a map. They can be accessed with :xmin, :xmax, :ymin, :ymax keywords."
  []
  {:xmin (find-extreme-point min first)
   :xmax (find-extreme-point max first)
   :ymin (find-extreme-point min second)
   :ymax (find-extreme-point max second)})

(def memoized-create-edge-points (memoize find-extreme-points))

(defn create-scan-grid
  "Creates a vector of all coordinates which belong to a rectangle
  area that includes all input coordinates."
  []
  (let [{:keys [xmin xmax ymin ymax]} (memoized-create-edge-points)]
    (for [x (range xmin (inc xmax))
          y (range ymin (inc ymax))]
      [x y])))

(def memoized-create-scan-grid (memoize create-scan-grid))

; --------------------------
; problem 1

(defn find-nearest-point
  "Finds the nearest input coordinate to (x,y) using the manhattan distance
  metric. Returns either the coordinate vector or -1 if there are more
  than 1 nearest coordinates."
  [[x y]]
  (let [distances (mapv #(manhattan-dist [x y] %) coordinates)
        min-distance (apply min distances)]
    (loop [[distance & rest-distances] distances
           index 0
           nearest-coordinate-index -1]
      (if distance
        (if (= distance min-distance)
          (if (>= nearest-coordinate-index 0)
            -1
            (recur rest-distances (inc index) index))
          (recur rest-distances (inc index) nearest-coordinate-index))
        (get coordinates nearest-coordinate-index)))))

(defn find-nearest-points
  "Find the nearest coordinates to each of the input coordinates.
  Returns a map. Each keys is an input coordinate and the corresponding
  value is a vector containing all nearest coordinates to the input
  coordinate."
  []
  (reduce (fn [result coordinate]
            (let [nearest-point (find-nearest-point coordinate)]
              (if (= nearest-point -1)
                result
                (update result nearest-point #(conj % coordinate)))))
          (zipmap coordinates (repeat [])) (memoized-create-scan-grid)))

(defn area-bounded?
  "Returns true iff the area coordinates create a bounded area.
  This only happens when a coordinate does not belong to the edge points
  of the rectangle with top-left coordinate (xmin, ymin) and bottom-right
  coordinate (xmax, ymax)."
  [area-points [xmin xmax ymin ymax]]
  (every? #(and (not= xmin (first %))
                (not= xmax (first %))
                (not= ymin (second %))
                (not= ymax (second %)))
          area-points))

(defn find-largest-area
  "Finds the largest bounded area."
  []
  (let [{:keys [xmin xmax ymin ymax]} (memoized-create-edge-points)
        edge-points [xmin xmax ymin ymax]]
    (reduce (fn [result [_ area-points]]
              (if (and (> (count area-points) result)
                       (area-bounded? area-points edge-points))
                (count area-points)
                result))
            0 (find-nearest-points))))

; --------------------------
; problem 2

(defn find-region-size
  "Finds the size of the region containing all locations which have a total
  distance to all given coordinates of less than 10000."
  []
  (reduce (fn [result point]
            (let [point-distances (map #(manhattan-dist % point) coordinates)]
              (if (< (apply + point-distances) 10000)
                (inc result)
                result)))
          0 (memoized-create-scan-grid)))

; --------------------------
; results

(defn day05-1
  []
  (find-largest-area))

(defn day05-2
  []
  (find-region-size))

(defn -main
  []
  (println (day05-1))
  (println (day05-2)))

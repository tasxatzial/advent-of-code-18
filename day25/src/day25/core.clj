(ns day25.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  [s]
  (->> s
       clojure.string/split-lines
       (mapv #(read-string (str \[ % \])))))

(def points (parse (slurp input-file)))

(defn distance
  "Returns the manhattan distance between p1 and p2."
  [p1 p2]
  (->> (map #(- %1 %2) p1 p2)
       (map #(Math/abs ^int %))
       (apply +)))

(defn find-distances
  "Returns a map of distances between a points and all points
  {i-th point -> distances to all points}."
  []
  (reduce (fn [result i]
            (let [point (get points i)
                  distances (map #(distance point %) points)]
              (assoc result i distances)))
          {} (range (count points))))

; --------------------------
; problem 1

(defn find-close-points
  "Returns a collection of points (their indexes) that are close to point at given index."
  [index distances-map]
  (let [point-distances (get distances-map index)
        distances (zipmap (range (count point-distances)) point-distances)]
    (map first (filter #(<= (second %) 3) distances))))

(defn constellation-points
  "Returns a set of all points (their indexes) that belong to the same constellation
  as the point at given index."
  ([index distances-map]
   (constellation-points [index] (set [index]) distances-map))
  ([last-added points distances-map]
   (let [close-points (->> last-added
                           (map #(find-close-points % distances-map))
                           flatten
                           (remove points))]
     (if (empty? close-points)
       points
       (recur close-points (into points close-points) distances-map)))))

(defn -main
  []
  (println (constellation-points 0 (find-distances))))

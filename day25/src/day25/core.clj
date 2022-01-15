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

(defn -main
  []
  (println (find-distances)))

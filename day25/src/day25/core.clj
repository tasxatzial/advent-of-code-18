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
  {point (index) -> distances to all points}."
  []
  (reduce (fn [result i]
            (let [point (get points i)
                  distances (mapv #(distance point %) points)]
              (assoc result i distances)))
          {} (range (count points))))

(defn find-connections
  "Returns a map of {point (index) -> points (indexes) in the same constellation}.
  Receives the distances map that is returned by find-distances."
  [distances-map]
  (reduce (fn [result [index distances]]
            (assoc result index (->> distances
                                     (zipmap (range (count distances)))
                                     (filter #(<= (second %) 3))
                                     (mapv first))))
          {} distances-map))

; --------------------------
; problem 1

(defn find-constellation-indexes
  "Returns a set of all points (indexes) that belong to the same constellation
  as the point at given index."
  ([index connections]
   (find-constellation-indexes [index] (set [index]) connections))
  ([last-added constellation-indexes connections]
   (let [extra-indexes (->> last-added
                         (map #(get connections %))
                         flatten
                         (remove constellation-indexes))]
     (if (empty? extra-indexes)
       constellation-indexes
       (let [new-indexes (into constellation-indexes extra-indexes)]
         (recur new-indexes new-indexes connections))))))

(defn find-constellations
  "Returns a collection of all constellations, each is a set of the indexes of the
  points that belong to the constellation."
  ([]
   (let [connections (find-connections (find-distances))
         indexes (set (range (count points)))]
     (find-constellations indexes [] connections)))
  ([indexes result connections]
   (if (empty? indexes)
     result
     (let [index (first indexes)
           constellation-indexes (find-constellation-indexes index connections)
           new-constellations (conj result constellation-indexes)
           new-indexes (reduce #(disj %1 %2) indexes constellation-indexes)]
       (recur new-indexes new-constellations connections)))))

; --------------------------
; results

(defn day25-1
  []
  (count (find-constellations)))

(defn -main
  []
  (println (time (day25-1))))

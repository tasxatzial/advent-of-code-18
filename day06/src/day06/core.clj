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

(defn- find-edge
  "Finds the {min,max} {x,y} coordinates.
  fn-extreme must be 'min' or 'max'.
  fn-coordinate must be 'first' (for the x coordinates) or 'second' (for the y coordinates)."
  [fn-extreme fn-coordinate]
  (->> coordinates
       (mapv fn-coordinate)
       (apply fn-extreme)))

(defn find-max-x
  "Finds the max x coordinate."
  []
  (find-edge max first))

(defn find-min-x
  "Finds the min x coordinate."
  []
  (find-edge min first))

(defn find-max-y
  "Finds the max y coordinate."
  []
  (find-edge max first))

(defn find-min-y
  "Finds the min y coordinate."
  []
  (find-edge min second))

(def memoized-find-max-x (memoize find-max-x))
(def memoized-find-min-x (memoize find-min-x))
(def memoized-find-max-y (memoize find-max-y))
(def memoized-find-min-y (memoize find-min-y))

(defn -main
  []
  (println (memoized-find-min-y)))

(ns day22.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(def depth 7863)
(def target-loc [14 760])

(declare memoized-find-geologic-index)
(declare memoized-find-erosion-level)

(defn find-geologic-index
  "Finds the geologic index at [x y]."
  [[x y :as loc]]
  (cond
    (and (zero? x) (zero? y)) 0
    (= loc target-loc) 0
    (zero? y) (* x 16807)
    (zero? x) (* y 48271)
    :else (* (memoized-find-erosion-level [(dec x) y])
             (memoized-find-erosion-level [x (dec y)]))))

(def memoized-find-geologic-index (memoize find-geologic-index))

(defn find-erosion-level
  "Finds the erosion level at loc."
  [loc]
  (let [geologic-index (memoized-find-geologic-index loc)]
    (rem (+ geologic-index depth) 20183)))

(def memoized-find-erosion-level (memoize find-erosion-level))

(defn find-location-type
  "Returns the type of a location based on the given erosion level."
  [erosion-level]
  (case (mod erosion-level 3)
    0 :rocky
    1 :wet
    2 :narrow))

; --------------------------
; problem 1

;; risk level of a location
(def region-type->risk-level
  {:rocky 0
   :wet 1
   :narrow 2})

; --------------------------
; results

(defn day22-1
  []
  (let [grid (for [i (range 0 (inc (first target-loc)))
                   j (range 0 (inc (second target-loc)))]
               [i j])]
    (->> grid
         (map #(memoized-find-erosion-level %))
         (map find-location-type)
         (map region-type->risk-level)
         (apply +))))

(defn -main
  []
  (println (day22-1)))

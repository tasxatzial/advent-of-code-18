(ns day22.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(def depth 7863)
(def target-loc [14 760])

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
(def location-type->risk-level
  {:rocky 0
   :wet 1
   :narrow 2})

(defn create-grid
  []
  (for [i (range 0 (inc (first target-loc)))
        j (range 0 (inc (second target-loc)))]
    [i j]))

; --------------------------
; problem 1, approach 1: memoization and mutual recursion

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

; --------------------------
; problem 1, approach 2: no memoization, no mutual recursion

(defn find-geologic-index2
  "Finds the geologic index at [x y]."
  [[x y :as loc] erosion-levels]
  (cond
    (and (zero? x) (zero? y)) 0
    (= loc target-loc) 0
    (zero? y) (* x 16807)
    (zero? x) (* y 48271)
    :else (let [erl1 (get erosion-levels [(dec x) y])
                erl2 (get erosion-levels [x (dec y)])]
            (* erl1 erl2))))

(defn find-erosion-level2
  "Finds the erosion level of all [x y] grid locations. Takes advantage of the
  order of the grid locations to avoid unnecessary calculations."
  [grid]
  (reduce (fn [[erosion-levels geologic-indexes] loc]
            (let [geologic-index (find-geologic-index2 loc erosion-levels)
                  erosion-level (rem (+ geologic-index depth) 20183)
                  new-erosion-levels (assoc erosion-levels loc erosion-level)
                  new-geologic-indexes (assoc geologic-indexes loc geologic-index)]
              [new-erosion-levels new-geologic-indexes]))
          [{} {}] grid))

; --------------------------
; results

(defn day22-1-sol1
  []
  (->> (create-grid)
       (map #(memoized-find-erosion-level %))
       (map find-location-type)
       (map location-type->risk-level)
       (apply +)))

(defn -main
  []
  (println (time (day22-1-sol1))))

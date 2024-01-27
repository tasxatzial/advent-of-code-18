(ns day05.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(def memoized-input-file->polymer (fn [] (butlast (slurp input-file))))

; --------------------------
; problem 1

(defn react?
  "Returns true if two units can react, false otherwise."
  [l1 l2]
  (= 32 (Math/abs (- (int l1) (int l2)))))

(defn find-fully-reacted-polymer
  "Returns the polymer after all possible reactions."
  [polymer]
  (reverse (reduce (fn [result letter]
                     (if (and (seq result) (react? letter (first result)))
                       (rest result)
                       (conj result letter)))
                   (list (first polymer))
                   (rest polymer))))

; --------------------------
; problem 2

(defn reverse-case
  "Transforms a lowercase unit to uppercase and vice versa."
  [letter]
  (if (>= (int letter) (int \a))
    (char (- (int letter) 32))
    (char (+ (int letter) 32))))

(defn generate-all-units
  "Returns a seq of all lowercase units."
  []
  (map char (range (int \a) (inc (int \z)))))

(defn find-fully-reacted-polymer-after-unit-removal
  "Removes the given unit from the polymer (case-insensitive), and returns
  the polymer after all possible reactions."
  [unit]
  (let [polymer (memoized-input-file->polymer)
        polymer-without-unit (remove #{unit (reverse-case unit)} polymer)]
    (find-fully-reacted-polymer polymer-without-unit)))

; --------------------------
; results

(defn day05-1
  []
  (-> (memoized-input-file->polymer)
      find-fully-reacted-polymer
      count))

(defn day05-2
  []
  (->> (generate-all-units)
       (map find-fully-reacted-polymer-after-unit-removal)
       (map count)
       (apply min)))

(defn -main
  []
  (println (day05-1))
  (println (day05-2)))

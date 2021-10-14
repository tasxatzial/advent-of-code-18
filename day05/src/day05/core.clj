(ns day05.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(def polymer (butlast (slurp input-file)))

; --------------------------
; problem 1

(defn react?
  "Returns true if two letters can react, false otherwise."
  [l1 l2]
  (= 32 (Math/abs (- (int l1) (int l2)))))

(defn react-polymer
  "Return the polymer sequence after all possible reactions."
  [polymer]
  (reverse (reduce (fn [result letter]
            (if (and (seq result) (react? letter (first result)))
              (rest result)
              (conj result letter)))
          (list (first polymer)) (rest polymer))))

; --------------------------
; problem 2

(defn reverse-case
  "Transforms a lowercase char to its uppercase equivalent and vice versa."
  [letter]
  (if (>= (int letter) (int \a))
    (char (- (int letter) 32))
    (char (+ (int letter) 32))))

(defn remove-unit
  "Removes the given unit (regardless of polarity) from the polymer sequence."
  [unit]
  (remove #{unit (reverse-case unit)} polymer))

(defn react-polymer-nounits
  "For each unit from \\a to \\z it returns the polymer sequence after the
  unit has been removed from the polymer and the polymer has fully reacted."
  []
  (let [units (map char (range (int \a) (inc (int \z))))]
    (map #(react-polymer (remove-unit %)) units)))

; --------------------------
; results

(defn day05-1
  []
  (count (react-polymer polymer)))

(defn day05-2
  []
  (apply min (map count (react-polymer-nounits))) )

(defn -main
  []
  (println (day05-1))
  (println (day05-2)))

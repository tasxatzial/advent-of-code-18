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
  "Return the polymer after all possible reactions."
  []
  (reverse (reduce (fn [result letter]
            (if (and (seq result) (react? letter (first result)))
              (rest result)
              (conj result letter)))
          (list (first polymer)) (rest polymer))))

; --------------------------
; results

(defn day05-1
  []
  (count (react-polymer)))

(defn -main
  []
  (println (day05-1)))

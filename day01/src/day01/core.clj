(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-file
  "Reads and parses the input file into a vector of frequencies."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (mapv #(Integer/parseInt %))))

(def memoized-input-file->frequency-changes (memoize parse-file))

; --------------------------
; problem 2

(defn get-first-repeated-freq
  "Finds the first frequency that is repeated twice."
  []
  (loop [[change & rest-changes] (memoized-input-file->frequency-changes)
         sums #{}
         curr-sum 0]
    (if change
      (let [new-sum (+ change curr-sum)]
        (if (contains? sums new-sum)
          new-sum
          (recur rest-changes (conj sums new-sum) new-sum)))
      (recur (memoized-input-file->frequency-changes) sums curr-sum))))

; ---------------------------------------
; results

(defn day01-1
  []
  (reduce + (memoized-input-file->frequency-changes)))

(defn day01-2
  []
  (get-first-repeated-freq))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))

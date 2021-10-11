(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n and converts it into a vector of numbers."
  [s]
  (->> s
       clojure.string/split-lines
       (mapv #(Integer/parseInt %))))

(def frequency-changes (parse (slurp input-file)))

; --------------------------
; problem 2

(defn first-repeated-freq
  "Finds the first frequency that is repeated twice."
  ([] (first-repeated-freq #{0} 0 frequency-changes))
  ([frequencies curr-frequency [frequency-change & rest-changes]]
   (if frequency-change
     (let [new-frequency (+ curr-frequency frequency-change)]
       (if (contains? frequencies new-frequency)
         new-frequency
         (recur (conj frequencies new-frequency) new-frequency rest-changes)))
     (recur frequencies curr-frequency frequency-changes))))

; ---------------------------------------
; results

(defn day01-1
  []
  (apply + frequency-changes))

(defn day01-2
  []
  (first-repeated-freq))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))

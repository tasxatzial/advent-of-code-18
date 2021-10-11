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

(defn -main
  []
  (println frequency-changes))

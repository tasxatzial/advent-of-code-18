(ns day07.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and converts it to a vector of instructions.
  Each instruction is a vector of two items that indicate the step letters
  in each input line."
  [s]
  (->> s
       (clojure.string/split-lines)
       (mapv #(vector (nth % 5) (nth % 36)))))

(def steps (parse (slurp input-file)))

(defn -main
  []
  (println steps))

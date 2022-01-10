(ns day18.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and returns the lumber area represented as a vector of vectors.
  Each vector represents a line in the input file.
  The contents of [i j] can be retrieved by get-in."
  [s]
  (let [split-lines (clojure.string/split-lines s)]
    (mapv vec split-lines)))

(def acres (parse (slurp input-file)))

(defn -main
  []
  (println acres))

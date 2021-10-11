(ns day03.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses a string representing a single claim."
  [line]
  (let [no-delims (clojure.string/split line #" @ |,|: |x")]
    (mapv #(Integer/parseInt %) (rest no-delims))))

(defn parse
  "Splits the input string by \n and converts it to an appropriate data
  structure."
  [s]
  (mapv parse-line (clojure.string/split-lines s)))

(def claims (parse (slurp input-file)))

(defn -main
  []
  (println claims))

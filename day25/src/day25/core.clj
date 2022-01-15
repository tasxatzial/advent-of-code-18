(ns day25.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  [s]
  (->> s
       clojure.string/split-lines
       (mapv #(read-string (str \[ % \])))))

(def points (parse (slurp input-file)))

(defn -main
  []
  (println points))

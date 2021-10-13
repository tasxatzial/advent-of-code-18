(ns day05.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(def polymer (slurp input-file))

(defn -main
  []
  (println polymer))

(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n and converts it into a vector of strings."
  [s]
  (clojure.string/split-lines s))

(def box-ids (parse (slurp input-file)))

; --------------------------
; problem 1

(defn find-letters
  "Finds the letters in the given box-id that appear exactly freq times."
  [freq box-id]
  (->> box-id
       frequencies
       (filter #(= freq (second %)))
       (map first)))

(defn count-freq
  "Counts the box-ids in the collection that have a letter that
  appears exactly freq times."
  [freq]
  (count (filter seq (map #(find-letters freq %) box-ids))))

(defn -main
  []
  (println (count-freq 2)))

(ns day13.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn track-type
  "Returns a keyword representing the track type."
  [ch]
  (case ch
    \- :horizontal
    \| :vertical
    \+ :intersect
    \/ :slash
    \\ :backslash
    \space :space
    (\> \< \v \^) :cart))

(defn parse-line
  "Parses the ith-line of the input file and creates an appropriate structure.
  Returns a seq of [[x y] track-type]"
  [i s]
  (let [parsed (map #(vector [%2 i] (track-type %1)) s (range (count s)))]
    (filter #(not= :space (second %)) parsed)))

(defn parse
  "Parses the input string and creates an appropriate structure.
  Returns a map of [[x y] track-type]"
  [s]
  (let [split-lines (clojure.string/split-lines s)
        parsed (map #(parse-line %1 %2) (range (count split-lines)) split-lines)]
    (reduce into {} parsed)))

(def tracks (parse (slurp input-file)))

(defn -main
  []
  (println tracks))

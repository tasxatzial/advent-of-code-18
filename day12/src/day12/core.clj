(ns day12.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")
(def init-state "##.#.####..#####..#.....##....#.#######..#.#...........#......##...##.#...####..##.#..##.....#..####")

(defn parse-rules
  [s]
  (let [data (clojure.string/split-lines s)
        rules (drop 2 data)]
    (into {} (map #(clojure.string/split % #" => ") rules))))

(def rules (parse-rules (slurp input-file)))

(defn -main
  []
  (println rules))

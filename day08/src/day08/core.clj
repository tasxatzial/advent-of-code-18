(ns day08.core
  (:gen-class))

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and converts it to a vector of numbers."
  [s]
  (let [tree-string (first (clojure.string/split-lines s))]
    (->> (clojure.string/split tree-string #" ")
         (mapv #(Integer/parseInt %)))))

(def tree (parse (slurp input-file)))

(defn -main
  []
  (println tree))

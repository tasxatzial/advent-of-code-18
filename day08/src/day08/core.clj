(ns day08.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and converts it to a vector of numbers."
  [s]
  (let [tree-string (first (clojure.string/split-lines s))]
    (->> (clojure.string/split tree-string #" ")
         (mapv #(Integer/parseInt %)))))

(def tree (parse (slurp input-file)))

; --------------------------
; problem 1

(defn metadata-sum
  "Computes the sum of all metadata."
  ([]
   (metadata-sum 0))
  ([header-index]
   (let [child-count (get tree header-index)]
     (loop [sum 0
            i header-index
            child-count child-count]
       (if (zero? child-count)
         (let [meta-count (get tree (inc header-index))
               meta-sum (apply + (subvec tree (+ i 2) (+ (+ i 2) meta-count)))]
           [(+ sum meta-sum) (+ i meta-count)])
         (let [[new-sum new-index] (metadata-sum (+ i 2))]
           (recur (+ sum new-sum) new-index (dec child-count))))))))

; --------------------------
; results

(defn day08-1
  []
  (metadata-sum))

(defn -main
  []
  (println (day08-1)))

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

(defn construct-tree
  "Returns a vector that explicitly indicates the tree structure.
  The vector has two items, the first one is the tree."
  ([]
   (construct-tree 0))
  ([header-index]
   (let [child-count (get tree header-index)]
     (loop [sum (conj [] [child-count (get tree (inc header-index))])
            i header-index
            child-count child-count]
       (if (zero? child-count)
         (let [meta-count (get tree (inc header-index))]
           [(into sum (subvec tree (+ i 2) (+ (+ i 2) meta-count))) (+ i meta-count)])
         (let [[new-sum new-index] (construct-tree (+ i 2))]
           (recur (conj sum new-sum) new-index (dec child-count))))))))

(def memoized-construct-tree (memoize construct-tree))

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

(defn metadata-sum2
  "Computes the sum of all metadata."
  ([]
   (let [tree-struct (first (memoized-construct-tree))]
     (metadata-sum2 tree-struct)))
  ([node]
   (let [[child-count meta-count] (first node)
         meta-sum (apply + (subvec node (- (count node) meta-count)))]
     (if (zero? child-count)
       meta-sum
       (loop [i 1
              sum meta-sum]
         (if (= i (inc child-count))
           sum
           (let [child-meta-sum (metadata-sum2 (get node i))]
             (recur (inc i) (+ sum child-meta-sum)))))))))
; --------------------------
; problem 2

(defn root-value
  "Computes the value of the root node of the tree."
  ([]
   (let [tree-struct (first (memoized-construct-tree))]
     (root-value tree-struct)))
  ([node]
   (if (vector? node)
     (let [[child-count meta-count] (first node)]
       (if (zero? child-count)
         (apply + (subvec node 1))
         (let [meta (subvec node (- (count node) meta-count))]
           (loop [[first-meta & rest-meta] (group-by identity meta)
                  sum 0]
             (if first-meta
               (let [first-meta-count (count (second first-meta))
                     first-meta-val (first first-meta)
                     child-val (root-value (get node first-meta-val))
                     new-sum (+ sum (* first-meta-count child-val))]
                 (recur rest-meta new-sum))
               sum)))))
     0)))

; --------------------------
; results

(defn day08-1a
  []
  (first (metadata-sum)))

(defn day08-1b
  []
  (first (metadata-sum2)))

(defn day08-2
  []
  (root-value))

(defn -main
  []
  (println (day08-1a))
  (println (day08-1b))
  (println (day08-2)))

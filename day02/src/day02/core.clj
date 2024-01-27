(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn get-ids
  "Reads and parses the input file into a vector of strings."
  []
  (->> input-file
       slurp
       clojure.string/split-lines))

(def memoized-get-ids (memoize get-ids))

; --------------------------
; problem 1

(defn find-letters
  "Finds the letters in the given id that appear exactly freq-count times."
  [freq-count id]
  (->> id
       frequencies
       (filter #(= freq-count (second %)))
       (map first)))

(defn count-ids
  "Finds the number of ids that have a letter that
  appears exactly freq-count times."
  [freq-count]
  (->> (memoized-get-ids)
       (map #(find-letters freq-count %))
       (filter seq)
       count))

(defn calc-checksum
  "Calculates the checksum."
  []
  (* (count-ids 2) (count-ids 3)))

; --------------------------
; problem 2

(defn differ-by-one?
  "Returns true if the given ids differ by exactly one character
  at the same position, else false."
  [id1 id2]
  (= 1 (count (filter false? (map = id1 id2)))))

(defn find-common-str
  "Returns the string formed by the chars which are common
  at the same positions in both ids."
  [id1 id2]
  (apply str (remove false? (map #(and (= %1 %2) %1) id1 id2))))

(defn- find-common-str-between-correct-ids1
  [id1 ids]
  (loop [[id2 & rest-ids] ids]
    (when id2
      (if (differ-by-one? id1 id2)
        (find-common-str id1 id2)
        (recur rest-ids)))))

(defn find-common-str-between-correct-ids
  "Returns the string formed by the chars which are common at the
  same positions in the two correct ids."
  []
  (loop [[id & rest-ids] (memoized-get-ids)]
    (when id
      (or (find-common-str-between-correct-ids1 id rest-ids)
           (recur rest-ids)))))

; ---------------------------------------
; results

(defn day02-1
  []
  (calc-checksum))

(defn day02-2
  []
  (find-common-str-between-correct-ids))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))

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

(defn calc-checksum
  "Calculates the required checksum."
  []
  (let [count-exactly-2times (count-freq 2)
        count-exactly-3times (count-freq 3)]
    (* count-exactly-2times count-exactly-3times)))

; --------------------------
; problem 2

(defn compare-ids
  "Returns a list containing true and false, each item indicates
  whether id1 and id2 are equal in the corresponding position."
  [id1 id2]
  (map = id1 id2))

(defn find-single-difference
  "If there is an item in all-ids which differs from id by exactly
  one character, it returns a list of their positional differences
  as returned by function compare-ids. That list should have exactly
  one false item. Returns nil if such item does not exist."
  [id all-ids]
  (let [id-comparisons (map #(compare-ids id %) all-ids)]
    (some #(and (= 1 (count (filter false? %))) %)
          id-comparisons)))

(defn find-single-difference-id
  "Finds the id that differs by exactly one character from an
  id in the box-ids and returns of vector of the id and the positional
  difference list as returned by compare-ids."
  []
  (loop [[id & rest-ids] box-ids]
    (if id
      (if-let [res (find-single-difference id rest-ids)]
        [id res]
        (recur rest-ids)))))

; ---------------------------------------
; results

(defn day02-1
  []
  (calc-checksum))

(defn day02-2
  []
  (let [[id diff] (find-single-difference-id)]
    (->> (map vector id diff)
         (filter #(true? (second %)))
         (map first)
         (apply str))))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))

(ns day16.core
  (:gen-class)
  (:require [clojure.set :refer [intersection]]))

;; opcode functions, see problem description for explanations on how they work
(defn addr
  [[_ A B C] registers]
  (let [res (+ (get registers A) (get registers B))]
    (assoc registers C res)))

(defn addi
  [[_ A B C] registers]
  (let [res (+ (get registers A) B)]
    (assoc registers C res)))

(defn mulr
  [[_ A B C] registers]
  (let [res (* (get registers A) (get registers B))]
    (assoc registers C res)))

(defn muli
  [[_ A B C] registers]
  (let [res (* (get registers A) B)]
    (assoc registers C res)))

(defn banr
  [[_ A B C] registers]
  (let [res (bit-and (get registers A) (get registers B))]
    (assoc registers C res)))

(defn bani
  [[_ A B C] registers]
  (let [res (bit-and (get registers A) B)]
    (assoc registers C res)))

(defn borr
  [[_ A B C] registers]
  (let [res (bit-or (get registers A) (get registers B))]
    (assoc registers C res)))

(defn bori
  [[_ A B C] registers]
  (let [res (bit-or (get registers A) B)]
    (assoc registers C res)))

(defn setr
  [[_ A _ C] registers]
  (assoc registers C (get registers A)))

(defn seti
  [[_ A _ C] registers]
  (assoc registers C A))

(defn gtir
  [[_ A B C] registers]
  (if (> A (get registers B))
    (assoc registers C 1)
    (assoc registers C 0)))

(defn gtri
  [[_ A B C] registers]
  (if (> (get registers A) B)
    (assoc registers C 1)
    (assoc registers C 0)))

(defn gtrr
  [[_ A B C] registers]
  (if (> (get registers A) (get registers B))
    (assoc registers C 1)
    (assoc registers C 0)))

(defn eqir
  [[_ A B C] registers]
  (if (= A (get registers B))
    (assoc registers C 1)
    (assoc registers C 0)))

(defn eqri
  [[_ A B C] registers]
  (if (= (get registers A) B)
    (assoc registers C 1)
    (assoc registers C 0)))

(defn eqrr
  [[_ A B C] registers]
  (if (= (get registers A) (get registers B))
    (assoc registers C 1)
    (assoc registers C 0)))

(def opcodes
  [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

; --------------------------
; problem 1

(def input1 "resources\\samples.txt")

(defn parse-sample
  "Parses a collection of strings that represent a [before, instruction, after]
  sequence in the samples file and returns the sequence represented by a vector of
  3 vectors."
  [sample]
  (let [before (second (clojure.string/split (first sample) #": "))
        instr (str \[ (second sample) \])
        after (second (clojure.string/split (nth sample 2) #": "))]
    (mapv read-string [before instr after])))

(defn parse1
  "Parses the input string and returns a collection of samples.
  Each sample is a vector representing a [before, instruction, after] sequence
  in the samples file."
  [s]
  (->> s
       (clojure.string/split-lines)
       (filter #(not= "" %))
       (partition 3)
       (map parse-sample)))

(def samples (parse1 (slurp input1)))

(defn count-sample-candidates
  "Returns the number of opcodes that behave like the given sample."
  [[before instruction after :as sample]]
  (->> opcodes
       (map #(= (% instruction before) after))
       (filter true?)
       count))

; --------------------------
; problem 2

(def input2 "resources\\tests.txt")

(defn parse2
  "Parses the input string and returns a collection of vectors, each one representing
  an instruction in the tests file."
  [s]
  (let [split-lines (clojure.string/split-lines s)]
    (->> split-lines
         (map #(read-string (str \[ % \]))))))

(def tests (parse2 (slurp input2)))

(defn find-sample-candidates
  "Returns all opcode functions that behave like the given sample. If called with 0 arguments
  it returns the opcode functions for every sample."
  ([]
   (map find-sample-candidates samples))
  ([[before instruction after :as sample]]
   (->> opcodes
        (map #(and (= (% instruction before) after) %))
        (filter (complement false?))
        (vector (first instruction)))))

(defn find-opcode-candidates
  "Returns a map of {opcode-num -> candidate opcode functions} by parsing all samples.
  This map can be further reduced to the final map of {opcode-num -> opcode function}."
  ([]
   (let [candidates (find-sample-candidates)
         op-codes (range (count opcodes))]
     (->> op-codes
          (map #(find-opcode-candidates % candidates))
          (zipmap (range (count opcodes))))))
  ([op-code candidates]
   (->> candidates
        (filter #(= op-code (first %)))
        (map (comp set second))
        (apply intersection)
        seq)))

; --------------------------
; results

(defn day16-1
  []
  (->> samples
       (map count-sample-candidates)
       (filter #(>= % 3))
       count))

(defn -main
  []
  (println (day16-1))
  (println tests))

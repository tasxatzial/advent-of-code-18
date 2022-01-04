(ns day16.core
  (:gen-class))

(def input1 "resources\\samples.txt")

(defn parse-sample
  "Parses a collection of strings that represent a [before, instruction, after]
  sequence in the samples file and returns the sequence represented by a vector of
  3 vectors, each vector contains 4 numbers."
  [sample]
  (let [before (second (clojure.string/split (first sample) #": "))
        instr (str \[ (second sample) \])
        after (second (clojure.string/split (nth sample 2) #": "))]
    (mapv read-string [before instr after])))

(defn parse
  "Parses the input string and returns a collection of samples.
  Each sample is a vector representing a [before, instruction, after] sequence
  in the samples file."
  [s]
  (->> s
       (clojure.string/split-lines)
       (filter #(not= "" %))
       (partition 3)
       (map parse-sample)))

(def samples (parse (slurp input1)))

(defn -main
  []
  (println samples))

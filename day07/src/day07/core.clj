(ns day07.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and converts it to a vector of instructions.
  Each instruction is a vector of two items that indicate the step letters
  in each input line."
  [s]
  (->> s
       (clojure.string/split-lines)
       (mapv #(vector (nth % 5) (nth % 36)))))

(def steps (parse (slurp input-file)))

(defn create-after-steps-map
  "Returns a map with key:step, value:vector of steps
  that must be completed after step. The map contains each
  uppercase letter from A to Z as a key."
  []
  (reduce (fn [result [before-step after-step]]
            (if (contains? result before-step)
              (update result before-step #(conj % after-step))
              (assoc result before-step #{after-step})))
          {} steps))

(defn create-before-steps-map
  "Returns a map with key:step, value:vector of steps
  that must be completed before step. The map contains each
  uppercase letter from A to Z as a key."
  []
  (reduce (fn [result [before-step after-step]]
            (if (contains? result after-step)
              (update result after-step #(conj % before-step))
              (assoc result after-step #{before-step})))
          {} steps))

(def memoized-create-after-steps (memoize create-after-steps-map))
(def memoized-create-before-steps (memoize create-before-steps-map))

(defn -main
  []
  (println (memoized-create-before-steps)))

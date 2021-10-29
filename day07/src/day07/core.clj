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

(def memoized-create-after-steps-map (memoize create-after-steps-map))
(def memoized-create-before-steps-map (memoize create-before-steps-map))

; --------------------------
; problem 1

(defn find-initial-candidates
  "Returns a set of the steps that can be completed in the first iteration."
  []
  (let [after-steps (memoized-create-after-steps-map)
        before-steps (memoized-create-before-steps-map)]
    (reduce (fn [result [before-step _]]
              (if (get before-steps before-step)
                result
                (conj result before-step)))
            #{} after-steps)))

(defn candidate?
  "Returns true if the given step can be added to the list of candidates, false otherwise."
  [step curr-candidates before-steps-map]
  (and (empty? (get before-steps-map step))
       (not (contains? curr-candidates step))))

(defn remove-step
  "Traverses before-steps and for each item it updates its corresponding after-steps
  by removing the step from the after-steps. Returns the updated before-steps-map."
  [before-steps step before-steps-map]
  (reduce (fn [result before-step]
            (assoc result before-step (disj (get result before-step) step)))
          before-steps-map before-steps))

(defn add-candidates
  "Adds all candidates from steps to the curr-candidates. Returns the updated curr-candidates."
  [steps curr-candidates before-steps-map]
  (reduce (fn [result step]
            (if (candidate? step curr-candidates before-steps-map)
              (conj result step)
              result))
          curr-candidates steps))

(defn compute-sequence
  "Computes the correct order of step execution and returns the required string."
  []
  (let [after-steps-map (memoized-create-after-steps-map)
        before-steps-map (memoized-create-before-steps-map)
        initial-candidates (find-initial-candidates)]
    (loop [result []
           candidates initial-candidates
           before-steps-map before-steps-map]
      (if (seq candidates)
        (let [step (first candidates)
              tmp-candidates (disj candidates step)
              after-steps (get after-steps-map step)
              new-before-steps-map (remove-step after-steps step before-steps-map)
              new-candidates (add-candidates after-steps tmp-candidates new-before-steps-map)
              new-result (conj result step)]
          (recur new-result new-candidates new-before-steps-map))
        (apply str result)))))

; --------------------------
; results

(defn day07-1
  []
  (compute-sequence))

(defn -main
  []
  (println (day07-1)))

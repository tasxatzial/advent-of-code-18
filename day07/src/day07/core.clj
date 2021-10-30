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

(defn find-initial-candidates
  "Returns a set of the steps that can be completed in the first iteration."
  []
  (let [after-steps (memoized-create-after-steps-map)
        before-steps (memoized-create-before-steps-map)]
    (reduce (fn [result [before-step _]]
              (if (get before-steps before-step)
                result
                (conj result before-step)))
            (sorted-set) after-steps)))

; --------------------------
; problem 1

(defn candidate?
  "Returns true if the given step can be added to the list of candidates, false otherwise."
  [step curr-candidates before-steps-map]
  (and (empty? (get before-steps-map step))
       (not (contains? curr-candidates step))))

(defn remove-step
  "Traverses after-steps and for each item it updates its corresponding before-steps
  by removing the step from the before-steps. Returns the updated before-steps-map."
  [after-steps step before-steps-map]
  (reduce (fn [result after-step]
            (let [before-steps (get result after-step)
                  new-before-steps (disj before-steps step)]
              (assoc result after-step new-before-steps)))
          before-steps-map after-steps))

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
; problem 2

(defn idle-worker?
  "Returns true if a worker is idle, false otherwise."
  [worker]
  (or (= \space (second worker)) (= 0 (second (second worker)))))

(defn finished-worker?
  "Returns true if a worker has just finished a step."
  [worker]
  (and (not= \space (second worker)) (= 0 (second (second worker)))))

(defn add-candidates2
  "Adds all after-steps of the just finished steps to the current candidates."
  [workers candidates after-steps-map]
  (reduce (fn [result worker]
            (if (finished-worker? worker)
              (let [finished-step (first (second worker))]
                (into result (get after-steps-map finished-step)))
              result))
          candidates workers))

(defn update-before-steps-map
  "Remove from the values of the before-steps map the steps that have been
  completed in the current iteration."
  [workers before-steps-map after-steps-map]
  (reduce (fn [result worker]
            (if (finished-worker? worker)
              (let [finished-step (first (second worker))
                    after-steps (get after-steps-map finished-step)]
                (remove-step after-steps finished-step result))
              result))
          before-steps-map workers))

(defn assign-worker
  "Assigns a new-step to a worker."
  [[worker-id _] new-step]
  (let [time-taken (+ 60 (- (int new-step) 64))]
    (vector worker-id [new-step time-taken])))

(defn advance-workers
  "Find the next state of the workers."
  [workers]
  (reduce (fn [result [worker-id worker-status :as worker]]
            (if (= \space worker-status)
              (conj result worker)
              (let [step (first worker-status)
                    time (second worker-status)]
                (if (= 0 time)
                  (conj result (vector worker-id \space))
                  (conj result (vector worker-id [step (dec time)]))))))
          (sorted-map) workers))

(defn find-assignable-candidates
  "Returns a vector of the candidate steps that can be assigned to a worker at the current
  time iteration."
  [candidates before-steps-map]
  (reduce (fn [result candidate]
            (if (empty? (get before-steps-map candidate))
              (conj result candidate)
              result))
          [] candidates))

(defn delete-assigned-candidates
  "Deletes the candidate steps that have been assigned at the current time step
  from the list of candidate steps."
  [assigned-workers candidates]
  (let [worker-steps (set (map #(first (second %)) assigned-workers))]
    (reduce disj candidates worker-steps)))

(defn all-done
  "Returns true if no more time iterations can be done, false otherwise."
  [workers candidates]
  (and (every? idle-worker? workers) (empty? candidates)))

(defn assign-workers
  "Assign the accepted candidate steps to the idle workers."
  [accepted-candidates workers]
  (let [idle-workers (filterv idle-worker? workers)]
    (map assign-worker idle-workers accepted-candidates)))

(defn compute-total-time
  "Computes the total time required to complete all steps."
  []
  (let [after-steps-map (memoized-create-after-steps-map)
        before-steps-map (memoized-create-before-steps-map)
        initial-candidates (find-initial-candidates)]
    (loop [time 0
           workers (into (sorted-map) (zipmap (range 1 6) (repeat \space)))
           candidates initial-candidates
           before-steps-map before-steps-map]
      (let [updated-workers (advance-workers workers)
            new-before-steps-map (update-before-steps-map updated-workers before-steps-map after-steps-map)
            tmp-candidates (add-candidates2 updated-workers candidates after-steps-map)
            accepted-candidates (find-assignable-candidates tmp-candidates new-before-steps-map)
            assigned-workers (assign-workers accepted-candidates updated-workers)
            new-candidates (delete-assigned-candidates assigned-workers tmp-candidates)
            new-workers (into updated-workers assigned-workers)]
        (if (all-done new-workers new-candidates)
          time
          (recur (inc time) new-workers new-candidates new-before-steps-map))))))

; --------------------------
; results

(defn day07-1
  []
  (compute-sequence))

(defn day07-2
  []
  (compute-total-time))

(defn -main
  []
  (println (day07-1))
  (println (day07-2)))

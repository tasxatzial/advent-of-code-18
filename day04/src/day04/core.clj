(ns day04.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn process-record
  "Takes a string record and converts it to a vector that contains 3 items:
  1) The date string.
  2) The time as a vector of [hour minute].
  3) The final item is one of:
     -1 (guard falls asleep)
     -2 (guard awakes)
     positive number (guard id)"
  [s]
  (let [split-record (clojure.string/split s #"[\[\]# ]")
        date (second split-record)
        [hour minute] (clojure.string/split (get split-record 2) #":")
        new-hour (Integer/parseInt hour)
        new-minute (Integer/parseInt minute)]
    (if (= 9 (count split-record))
      (let [guard-id (Integer/parseInt (get split-record 6))]
        [date [new-hour new-minute] guard-id])
      (if (= "falls" (get split-record 4))
        [date [new-hour new-minute] -1]
        [date [new-hour new-minute] -2]))))

(defn parse
  "Parses the input string and converts it to a vector of records.
  The final data structure has the records sorted by date,time."
  [s]
  (->> s
       (clojure.string/split-lines)
       (sort)
       (mapv process-record)))

(def records (parse (slurp input-file)))

(defn contained?
  "Returns true if minutes is in [start end), false otherwise."
  [minute [start end]]
  (and (<= start minute) (< minute end)))

(defn find-minute-frequency
  "Finds in how many sleep periods the given minute appears in."
  [minute sleep-periods]
  (count (filter true? (map #(contained? minute %) sleep-periods))))

; --------------------------
; problem 1

(defn extract-sleep-periods
  "Takes a collection of records that indicate sleep/wake actions
  and returns a vector containing sleep periods.
  Example:
  [[\"1518-03-27\" [24 11] -1] [\"1518-03-27\" [24 57] -2]
  [\"1518-03-28\" [24 16] -1] [\"1518-03-28\" [24 33] -2]]
  will return [[11 57] [16 33]] which means sleep between
  11 and 57 minutes and sleep between 16 and 33 minutes."
  [records]
  (loop [[sleep-record awake-record & rest-records] records
         result []]
    (if sleep-record
      (let [period [(second (second sleep-record))
                    (second (second awake-record))]]
        (recur rest-records (conj result period)))
      result)))

(defn find-sleep-periods
  "Collects all sleep periods for every guard. Result is a map:
  1) keys = guard id
  2) value = sleep periods of the corresponding guard."
  []
  (loop [[record & rest-records] records
         guard-sleep-periods {}]
    (if record
      (let [sleep-awake-records (take-while #(or (= -1 (last %)) (= -2 (last %)))
                                         rest-records)
            remaining-records (drop (count sleep-awake-records) rest-records)
            sleep-periods (extract-sleep-periods sleep-awake-records)
            guard-id (last record)
            new-sleep-periods (update guard-sleep-periods guard-id #(into % sleep-periods))]
        (recur remaining-records new-sleep-periods))
      guard-sleep-periods)))

(defn find-total-sleep
  "Takes a collection of sleep periods and returns the total sleep time in minutes."
  [sleep-periods]
  (reduce (fn [result [start end]]
            (+ result (- end start)))
          0 sleep-periods))

(defn find-most-asleep-guard
  "Finds the guard that sleeps the most. Returns a vector that contains
  the guard id and the total sleep time of the guard."
  [sleep-periods-map]
  (reduce (fn [most-asleep-result [guard-id guard-sleep-periods]]
            (let [guard-sleep-time (find-total-sleep guard-sleep-periods)
                  most-asleep-time (second most-asleep-result)]
              (if (> guard-sleep-time most-asleep-time)
                [guard-id guard-sleep-time]
                most-asleep-result)))
          [0 0] sleep-periods-map))

(defn find-most-sleep-minute
  "Takes a collection of sleep periods and finds the most frequent minute.
  Returns a vector that contains the frequency and the minute."
  [sleep-periods]
  (reduce (fn [most-sleep-minute minute]
            (let [sleep-minute-frequency (find-minute-frequency minute sleep-periods)
                  max-frequency (first most-sleep-minute)]
              (if (> sleep-minute-frequency max-frequency)
                [sleep-minute-frequency minute]
                most-sleep-minute)))
          [0 0] (range 1 60)))

(def memoized-find-sleep-periods (memoize find-sleep-periods))

; --------------------------
; problem 2

(defn find-max-minute-frequency
  "Finds the minute that appears in most of the sleep periods.
  Returns a vector containing the minute and its frequency."
  [sleep-periods]
  (reduce (fn [[_ frequency-result :as result] minute]
            (let [minute-frequency (find-minute-frequency minute sleep-periods)]
              (if (> minute-frequency frequency-result)
                [minute minute-frequency]
                result)))
          [0 0] (range 1 60)))

; --------------------------
; results

(defn day04-1
  []
  (let [sleep-periods (memoized-find-sleep-periods)
        most-asleep-guard (find-most-asleep-guard sleep-periods)
        guard-id (first most-asleep-guard)
        guard-sleep-periods (get sleep-periods guard-id)
        guard-most-sleep-minute (find-most-sleep-minute guard-sleep-periods)]
    (* guard-id (second guard-most-sleep-minute))))

(defn -main
  []
  (println (day04-1)))

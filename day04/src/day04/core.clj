(ns day04.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn preprocess-record
  "Takes a record in string form, splits it into date & action parts
  and replaces the 00 hour with 24.
  Example:
  \"[1518-10-05 00:02] Guard #3347 begins shift\"
  becomes
  [\"1518-10-05 24:02\" \" Guard #3347 begins shift\"]"
  [s]
  (let [split-record (clojure.string/split s #"[\[\]]")
        record-data (filterv #(not= "" %) split-record)
        date-time (first record-data)
        new-date-time (clojure.string/replace date-time #"00:" "24:")]
    (assoc record-data 0 new-date-time)))

(defn postprocess-record
  "Takes a record in the form returned by preprocess-record and
  converts it to a vector that contains 3 items:
  1) The date string.
  2) The time as a vector of [hour minute].
  3) The final item is one of:
     -1 (guard falls asleep)
     -2 (guard awakes)
     positive number (guard id)"
  [[date-time action]]
  (let [[date time] (clojure.string/split date-time #" ")
        [hour minute] (clojure.string/split time #":")
        new-hour (Integer/parseInt hour)
        new-minute (Integer/parseInt minute)
        split-action (filterv #(not= "" %) (clojure.string/split action #"[# ]"))]
    (if (= 4 (count split-action))
      (let [guard-id (Integer/parseInt (second split-action))]
        [date [new-hour new-minute] guard-id])
      (if (= "falls" (first split-action))
        [date [new-hour new-minute] -1]
        [date [new-hour new-minute] -2]))))

(defn parse
  "Parses the input string and converts it to a vector of records.
  preprocess-record and postprocess-record are applied in succession
  to a string record. The final data structure has the records
  sorted by date,time."
  [s]
  (->> s
       (clojure.string/split-lines)
       (map preprocess-record)
       (sort-by first)
       (mapv postprocess-record)))

(def records (parse (slurp input-file)))

; --------------------------
; problem 1

(defn extract-sleep-periods
  "Takes a collection of records that indicate sleep/wake actions
  and returns a vector containing sleep periods.
  Example:
  [[\"1518-03-27\" [24 11] -1] [\"1518-03-27\" [24 57] -2]
  [\"1518-03-28\" [24 16] -1] [\"1518-03-28\" [24 33] -2]]
  will return [[11 56] [16 32]] which means sleep between
  11 and 56 minutes (inclusive) and sleep between
  16 and 32 minutes."
  [records]
  (loop [[sleep-record awake-record & rest-records] records
         result []]
    (if sleep-record
      (let [period [(second (second sleep-record))
                    (dec (second (second awake-record)))]]
        (recur rest-records (conj result period)))
      result)))

(defn find-sleep-periods
  "Collects all sleep periods for every guard. Result is a map containing
  keys (guard ids) and the corresponding collection of sleep periods as values."
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

(defn -main
  []
  (println (find-sleep-periods)))

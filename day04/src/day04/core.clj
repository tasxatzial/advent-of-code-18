(ns day04.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn preprocess-record
  "Takes a record in string form, splits it into tokens and replaces
  00 that appears in hour time with 24. Example:
  \"[1518-10-05 00:02] Guard #3347 begins shift\"
  becomes
  [\"1518-10-05\" \"24:02\" \"Guard\" \"3347\" \"begins\" \"shift\"]"
  [s]
  (let [split-record (clojure.string/split s #"[#\[\] ]")
        record-data (filterv #(not= "" %) split-record)
        time (second record-data)
        new-time (clojure.string/replace time #"00:" "24:")]
    (assoc record-data 1 new-time)))

(defn postprocess-record
  "Takes a record in the form returned by preprocess-record and
  converts it to an appropriate data structure:
  1) The date string remains as is.
  2) The time string is replaced by a vector of [hour minute].
  3) The rest is one of:
     -1 (guard falls asleep)
     -2 (guard awakes)
     positive number (guard id)"
  [[date time & rest-record]]
  (let [[hour minute] (clojure.string/split time #":")
        new-hour (Integer/parseInt hour)
        new-minute (Integer/parseInt minute)]
    (if (= 4 (count rest-record))
      (let [guard-id (Integer/parseInt (second rest-record))]
        [date [new-hour new-minute] guard-id])
      (if (= "falls" (first rest-record))
        [date [new-hour new-minute] -1]
        [date [new-hour new-minute] -2]))))

(defn -main
  []
  (println (postprocess-record ["1518-10-05" "24:02" "Guard" "3347" "begins" "shift"])))

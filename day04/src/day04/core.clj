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

(defn -main
  []
  (println (preprocess-record "[1518-10-05 00:02] Guard #3347 begins shift")))

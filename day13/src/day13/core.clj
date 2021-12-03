(ns day13.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn track-type
  "Returns a keyword representing the track type."
  [ch]
  (case ch
    \- :horizontal
    \| :vertical
    \+ :intersect
    \/ :slash
    \\ :backslash
    \space :space
    (\> \< \v \^) :cart))

(defn parse-line
  "Parses the ith-line of the input file and creates an appropriate structure.
  Returns a seq of [[x y] track-type]"
  [i s]
  (let [parsed (map #(vector [%2 i] (track-type %1)) s (range (count s)))]
    (filter #(not= :space (second %)) parsed)))

(defn parse
  "Parses the input string and creates an appropriate structure.
  Returns a map of [[x y] track-type]"
  [s]
  (let [split-lines (clojure.string/split-lines s)
        parsed (map #(parse-line %1 %2) (range (count split-lines)) split-lines)]
    (reduce into {} parsed)))

(def input-tracks (parse (slurp input-file)))

(defn get-neighbors
  "Finds the neighbors of a track located at [x y].
  Returns a 4 item vector of [[x y] track-type]. Neighbors are listed in the
  following order: top bottom left right."
  [[x y] tracks]
  (let [top (get tracks [x (dec y)])
        bottom (get tracks [x (inc y)])
        left (get tracks [(dec x) y])
        right (get tracks [(inc x) y])]
    [top bottom left right]))

(defn get-track-by-neighbors
  "Returns the track type at [x y] by examining its neighbors only.
  Returns a [[x y] track-type]."
  [[loc _] tracks]
  (let [[top bottom left right] (get-neighbors loc tracks)]
    (cond
      (and (or (= :vertical top) (= :intersect top))
           (or (= :horizontal left) (= :intersect left))
           (or (= :vertical bottom) (= :intersect bottom))
           (or (= :horizontal right) (= :intersect right))) [loc :intersect]
      (and (or (= :vertical top) (= :intersect top))
           (or (= :vertical bottom) (:intersect bottom))) [loc :vertical]
      (and (or (= :horizontal left) (= :intersect left))
           (or (= :horizontal right) (= :intersect right))) [loc :horizontal]
      (and (= :horizontal left) (= :vertical top)) [loc :slash]
      (and (= :horizontal right) (= :vertical bottom)) [loc :slash]
      (and (= :horizontal left) (= :vertical bottom)) [loc :backslash]
      (and (= :horizontal right) (= :vertical bottom)) [loc :backslash])))

(defn find-carts
  "Finds the tracks that are occupied by carts.
  Returns a seq of [[x y] :cart]"
  [tracks]
  (filter #(= :cart (second %)) tracks))

(defn replace-carts
  "Scans the initial parsed tracks and replaces all tracks that have a cart
  character with the correct track character."
  [tracks]
  (let [carts (find-carts tracks)
        replaced (map #(get-track-by-neighbors % tracks) carts)]
    (into tracks replaced)))

(def tracks (replace-carts input-tracks))

(defn -main
  []
  (println tracks))

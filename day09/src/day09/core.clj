(ns day09.core
  (:gen-class))

(def players 432)

; --------------------------
; problem 1

(defn marble-positions-after-del
  "Removes the appropriate marble from the positions vector."
  [marble-positions]
  (let [remove-pos (- (count marble-positions) 8)
        second-part (subvec marble-positions (+ remove-pos 2))
        first-part (subvec marble-positions 0 remove-pos)
        curr-marble (get marble-positions (inc remove-pos))]
    (conj (into second-part first-part) curr-marble)))

(defn marble-positions-after-add
  "Adds a marble to the appropriate position in the positions vector."
  [marble-positions marble]
  (conj (subvec marble-positions 1) (first marble-positions) marble))

(defn add-marble
  "Inserts a marble to the positions vector and returns a vector with 2 elements:
  1) The updated player scores.
  2) The updated vector of marbles."
  [marble scores marble-positions]
  (let [new-positions (marble-positions-after-add marble-positions marble)]
    [scores new-positions]))

(defn remove-marble
  "Removes a marble from the positions vector and returns a vector with 2 elements:
  1) The updated player scores.
  2) The updated vector of marbles."
  [marble scores marble-positions player]
  (let [removed-pos (- (count marble-positions) 8)
        removed-marble (get marble-positions removed-pos)
        new-positions (marble-positions-after-del marble-positions)
        player-score (get scores player 0)
        new-player-score (+ player-score removed-marble marble)
        new-scores (assoc scores player new-player-score)]
    [new-scores new-positions]))

(defn make-move
  "Returns the updated map of scores after a marble has been played."
  [marble scores marble-positions player]
  (if (zero? (mod marble 23))
    (remove-marble marble scores marble-positions player)
    (add-marble marble scores marble-positions)))

(defn calc-scores
  "Calculate the final map of player scores after the game has ended.
  Keys run from 0 to (players -1)."
  [last-marble]
  (if (= 1 last-marble)
    {}
    (loop [marble 2
           scores {}
           player 1
           positions (conj [] 0 1)]
      (if (= marble last-marble)
        scores
        (let [[new-scores new-positions] (make-move marble scores positions player)
              next-player (mod (inc player) players)
              next-marble (inc marble)]
          (recur next-marble new-scores next-player new-positions))))))

; --------------------------
; results

(defn day09-1
  []
  (apply max (map second (calc-scores 71019))))

(defn -main
  []
  (println (day09-1)))

(ns day09.core
  (:gen-class))

(def players 432)
(def last-marble 71019)

; --------------------------
; common

(defn vec-remove
  "Removes the element in position pos of a vector coll."
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn vec-add
  "Adds the element in position pos of a vector coll.
  If pos == (count coll) the element is added to the end of the vector."
  [pos coll elem]
  (into (conj (subvec coll 0 pos) elem) (subvec coll pos)))

; --------------------------
; problem 1

(defn make-move
  "Makes one move in the game and returns the update map of scores."
  [pos marble scores positions player]
  (if (zero? (mod marble 23))
    (remove-marble pos marble scores positions player)
    (add-marble pos marble scores positions)))

(defn calc-scores
  "Calculate a map of player scores. Keys run from 0 to (players -1)."
  []
  (loop [pos 0
         marble 1
         scores {}
         player 0
         positions [0]]
    (if (= marble last-marble)
      scores
      (let [[new-scores new-pos new-positions] (make-move pos marble scores positions player)
            next-player (mod (inc player) players)
            next-marble (inc marble)]
        (recur new-pos next-marble new-scores next-player new-positions)))))

; --------------------------
; results

(defn day09-1a
  []
  (apply max (map second (calc-scores))))

(defn -main
  []
  (println (calc-scores)))

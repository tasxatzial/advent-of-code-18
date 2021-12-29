(ns day14.core
  (:gen-class))

; --------------------------
; common

(def input 681901)
(def input-vec [6 8 1 9 0 1])
(def input-count (count input-vec))
(def starting-recipes [3 7])
(def elf1-pos 0)
(def elf2-pos 1)

(defn next-round
  "Accepts a vector of the already computed recipes and the positions of the
  elves and returns the new recipes and positions after one round."
  [recipes elf1-pos elf2-pos]
  (let [elf1-recipe (get recipes elf1-pos)
        elf2-recipe (get recipes elf2-pos)
        recipes-sum (+ elf1-recipe elf2-recipe)
        new-recipes (if (>= recipes-sum 10)
                      (conj recipes 1 (- recipes-sum 10))
                      (conj recipes recipes-sum))
        new-elf1-pos (rem (+ elf1-pos elf1-recipe 1) (count new-recipes))
        new-elf2-pos (rem (+ elf2-pos elf2-recipe 1) (count new-recipes))]
    [new-recipes new-elf1-pos new-elf2-pos]))

(defn simulate
  "Play the game until the condition specified by the given fn_stop function
  is met. Returns the final vector of recipes."
  ([fn_stop]
   (simulate fn_stop starting-recipes elf1-pos elf2-pos))
  ([fn-stop recipes elf1-pos elf2-pos]
   (loop [recipes recipes
          elf1-pos elf1-pos
          elf2-pos elf2-pos]
     (let [[new-recipes new-elf1-pos new-elf2-pos] (next-round recipes elf1-pos elf2-pos)]
       (if (fn-stop new-recipes)
         new-recipes
         (recur new-recipes new-elf1-pos new-elf2-pos))))))

; --------------------------
; problem 1

(def last-recipe-count 10)
(def max-recipe-count (+ input last-recipe-count))

(defn stop1
  "Returns true if the condition specified by the rules is met,
  false otherwise (problem 1)"
  [recipes]
  (>= (count recipes) max-recipe-count))

; --------------------------
; problem 2

(def recipe-batch-size 50)
(def start-search-offset (- (+ 1 recipe-batch-size input-count)))

(defn find-input
  "If the sequence of the input digits appears in the recipes vector, return
  the index of its first digit in the vector, else return nil.
  Only the indices from (+ (count recipes) start-search-offset) are considered."
  [recipes]
  (let [start-index (+ (count recipes) start-search-offset)
        end-index (inc (- (count recipes) input-count))]
    (loop [i start-index]
      (if (< i 0)
        (recur 0)
        (when (not= i end-index)
          (let [s (subvec recipes i (+ i input-count))]
            (if (= s input-vec)
              i
              (recur (inc i)))))))))

(defn stop2
  "Returns true if the condition specified by the rules is met,
  false otherwise (problem 2)"
  [recipes]
  (let [r (rem (count recipes) recipe-batch-size)]
    (and (or (= 0 r) (= 1 r))
         (find-input recipes))))

; --------------------------
; results

(defn day14-1
  []
  (let [recipes (simulate stop1)
        last-recipes (drop input recipes)]
    (if (= last-recipe-count (count last-recipes))
      last-recipes
      (butlast last-recipes))))

(defn -main
  []
  (println (day14-1)))

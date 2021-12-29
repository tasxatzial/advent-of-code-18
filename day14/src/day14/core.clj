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


(defn -main
  []
  (println (next-round starting-recipes elf1-pos elf2-pos)))

(ns day13.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn track-type
  "Returns a keyword representing the track type. If the character is a
  cart character, it returns a keyword describing the cart direction."
  [ch]
  (case ch
    \- :horizontal
    \| :vertical
    \+ :intersect
    \/ :slash
    \\ :backslash
    \space :space
    \> :cart-right
    \< :cart-left
    \v :cart-down
    \^ :cart-up))

(defn parse-line
  "Parses the ith-line of the input file and creates an appropriate structure.
  Returns a seq of [[x y] track-type]"
  [i s]
  (map #(vector [%2 i] (track-type %1))
       s (range (count s))))

(defn parse
  "Parses the input string and creates an appropriate structure.
  Returns a map of [[x y] track-type]"
  [s]
  (let [split-lines (clojure.string/split-lines s)
        parsed (map #(parse-line %1 %2)
                    (range (count split-lines)) split-lines)]
    (reduce into {} parsed)))

(def input-tracks (parse (slurp input-file)))

(defn get-neighbors
  "Finds the neighbors of a track located at [x y]. Neighbors are listed in the following order:
  top bottom left right. Returns a 4 item vector of [[x y] track-type]."
  [[x y] tracks]
  (let [top (get tracks [x (dec y)])
        bottom (get tracks [x (inc y)])
        left (get tracks [(dec x) y])
        right (get tracks [(inc x) y])]
    [top bottom left right]))

(defn get-track-by-neighbors
  "Returns the track type at [x y] by examining its direct neighbors only. If there is a cart on
  the track it still returns the correct type of the track. Assumes that none of the neighbors are
  occupied by carts thus the function can be used only on the initial tracks (before any cart movements).
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

(defn cart?
  "Returns true if the track-type represents a cart, false otherwise."
  [[_ track-type]]
  (or (= :cart-down track-type)
      (= :cart-left track-type)
      (= :cart-right track-type)
      (= :cart-up track-type)))

(defn find-carts
  "Finds the tracks that are occupied by carts.
  Returns a seq of [[x y] direction turn-choice].
  Direction & Turn-choice are one of: cart-down :cart-left :cart-right :cart-up.
  Turn-choice indicates what the cart should do when it arrives at an intersection."
  [tracks]
  (let [carts (filter #(cart? %) tracks)]
    (map #(vector (first %) (second %) :left) carts)))

(defn replace-carts
  "Scans the initial track info and replaces all tracks that have a cart
  character with the correct track character."
  [tracks]
  (let [carts (find-carts tracks)
        replaced (map #(get-track-by-neighbors % tracks) carts)]
    (into tracks replaced)))

(def tracks (replace-carts input-tracks))
(def carts (sort-by (juxt first second) (find-carts input-tracks)))

(defn next-loc
  "Returns the next location of a cart that is in [x y] and has the given direction."
  [[x y] cart-direction]
  (case cart-direction
    :cart-left [(dec x) y]
    :cart-right [(inc x) y]
    :cart-up [x (dec y)]
    :cart-down [x (inc y)]))

; simple mapping of the track types to integers
(def track-type-num
  {:horizontal 10
   :vertical 20
   :slash 30
   :backslash 40
   :intersect 50})

; simple mapping of the cart directions to integers
(def cart-direction-num
  {:cart-up 1
   :cart-down 2
   :cart-right 3
   :cart-left 4})

(defn next-direction
  "Returns the next direction of a cart that has just moved to a track of the given
  type. Turn-choice denotes how it should turn if next track type is an intersection."
  [next-track-type cart-direction turn-choice]
  (let [id (+ (track-type-num next-track-type) (cart-direction-num cart-direction))]
    (case id
      13 :cart-right
      14 :cart-left
      21 :cart-up
      22 :cart-down
      31 :cart-right
      32 :cart-left
      33 :cart-up
      34 :cart-down
      41 :cart-left
      42 :cart-right
      43 :cart-down
      44 :cart-up
      51 (case turn-choice
           :left :cart-left
           :right :cart-right
           :straight :cart-up)
      52 (case turn-choice
           :left :cart-right
           :right :cart-left
           :straight :cart-down)
      53 (case turn-choice
           :left :cart-up
           :right :cart-down
           :straight :cart-right)
      54 (case turn-choice
           :left :cart-down
           :right :cart-up
           :straight :cart-left))))

; what turn a cart should take when the previous turn is given
(def next-turn-choice
  {:left :straight
   :straight :right
   :right :left})

(defn update-cart
  "Updates the cart after 1 tick."
  [[[x y] cart-direction turn-choice]]
  (let [new-loc (next-loc [x y] cart-direction)
        new-track (get tracks new-loc)
        new-direction (next-direction new-track cart-direction turn-choice)]
    (if (= :intersect new-track)
      [new-loc new-direction (next-turn-choice turn-choice)]
      [new-loc new-direction turn-choice])))

(defn collided?
  "Returns true if the given location is also a location of one of the carts,
  false otherwise."
  [loc carts]
  (let [cart-locations (set (map first carts))]
    (contains? cart-locations loc)))

(defn move-carts-one-step
  "Updates the carts after one tick. If no collision happened, it returns
  the updated carts. Else it returns the location of the collision."
  [carts]
  (loop [[cart & rest-carts] carts
         new-carts []]
    (if cart
      (let [new-cart (update-cart cart)
            new-loc (first new-cart)]
        (if (collided? new-loc carts)
          new-loc
          (recur rest-carts (conj new-carts new-cart))))
      new-carts)))

; --------------------------
; problem 1

(defn simulate1
  "Runs the simulation for the given number of steps or until a collision has occurred.
  Returns nil if no collision has occurred after the given steps. Else it returns the
  location of the first collision."
  [steps carts]
  (when (pos? steps)
    (let [result (move-carts-one-step carts)]
      (if (coll? (first result))
        (recur (dec steps) result)
        result))))

; --------------------------
; results

(defn day08-1
  []
  (simulate1 200 carts))

(defn -main
  []
  (println (day08-1)))

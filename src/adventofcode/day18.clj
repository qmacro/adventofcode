(ns adventofcode.day18
  (:require [clojure.string :as str]))

(def input
  (->> "resources/day18input"
       slurp
       str/split-lines))

(def board
  "Take the raw input (a seq of strings with # and . chars)
  and produce a nested vector that will act as 2D lookupable
  board."
  (vec (map vec input)))

(defn rowsize
  "How many rows the board has."
  [board]
  (count board))

(defn colsize
  "How many cols the board has."
  [board]
  (count (first board)))

(def on \#)
(def off \.)

(defn on?
  [cell]
  (= cell on))

(def off? (complement on?))


(defn coords
  "Create seq of all the coordinates for the board."
  [board]
  (for [row (range (rowsize board))
        col (range (colsize board))]
    [row col]))

(def rel-nbr-coords
  "A seq representing the relative coordinate adjustments to work
  out the (max) 9 surrounding 2D neighbours of a cell, i.e.:
  ((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1)).
  Note that this seq contains (0 0) i.e. 'itself'."
  (for [row (range 3)
        col (range 3)]
    (map dec [row col])))

(defn cellstate
  "Return the state of a cell in the board,
  at the given row/col coord."
  [[r c] board]
  ((board r) c))

(defn out-of-bounds?
  "Whether a given cell is out of bounds
  with respect to the board edges."
  [[r c] board]
  (or (< r 0)
      (>= r (rowsize board))
      (< c 0)
      (>= c (colsize board))))

(defn neighbours
  "Returns the coords of the neighbours of a given cell,
  excluding the cell itself and any neighbours that are
  out of bounds."
  [[r c] board]
  (->> (map #(map + [r c] %) rel-nbr-coords)
       (filter #(not= [r c] %))
       (filter #(not (out-of-bounds? % board)))))

(defn neighbours-on
  "Returns how many neighbours of a given cell are on."
  [[r c] board]
  (->> (neighbours [r c] board)
       (map #(cellstate % board))
       (filter #(on? %))
       count))

(defn toggle
  "Toggles a light."
  [light]
  (case light
    on off
    off on))

(defn switch
  "Switches (or not) a light for the next state iteration,
  according to its current state, and how many of its 
  neighbours are on."
  [light n]
  (if (or (and (off? light) (= 3 n))
          (and (on? light) (and (not= 2 n) (not= 3 n))))
    (if (on? light) off on)
    light))

(defn re-form
  [flatboard c]
  (vec (map vec (partition c flatboard))))

(defn transform
  "Transform board n steps."
  [board n]
  (loop
    [b board
     i n]
    (if (zero? i)
      b
      (let [nbrs-on (->> (coords b) (map #(neighbours-on % b)))]
        (recur (re-form (map switch (flatten b) nbrs-on) (colsize b)) (dec i))))))

(count (filter on? (flatten (transform board 100))))


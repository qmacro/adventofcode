(ns adventofcode.day18
  (:require [clojure.string :as str]))

(def input
  (->> "resources/day18test"
       slurp
       str/split-lines))

(def board
  "Take the raw input (a seq of strings with # and . chars)
  and produce a nested vector that will act as 2D lookupable
  board."
  (vec (map vec input)))

(def rowsize
  "How many rows the board has."
  (count board))

(def colsize
  "How many cols the board has."
  (count (first board)))

(defn on?
  [cell]
  (= cell \#))

(def coords
  "Create seq of all the coordinates for the board."
  (for [row (range rowsize)
        col (range colsize)]
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
  [[r c]]
  ((board r) c))

(defn out-of-bounds?
  "Whether a given cell is out of bounds
  with respect to the board edges."
  [[r c]]
  (or (< r 0)
      (>= r rowsize)
      (< c 0)
      (>= c colsize)))

(defn neighbours
  "Returns the coords of the neighbours of a given cell,
  excluding the cell itself and any neighbours that are
  out of bounds."
  [[r c]]
  (->> (map #(map + [r c] %) rel-nbr-coords)
       (filter #(not= [r c] %))
       (filter #(not (out-of-bounds? %)))))

(defn neighbours-on
  "Returns how many neighbours of a given cell are on."
  [[r c]]
  (->> (neighbours [r c])
       (map cellstate)
       (filter #(on? %))
       count))



(ns adventofcode.day16
  (:require [clojure.string :as str]))

(def analysis
  {:children 3
   :cats 7
   :samoyeds 2
   :pomeranians 3
   :akitas 0
   :vizslas 0
   :goldfish 5
   :trees 3
   :cars 2
   :perfumes 1})

(defn pairmap
  "Turns a pair of two strings, where the second
  is an int, to a map.
  E.g. ('capacity', '-3') -> {:capacity -3}"
  [[s1 s2]]
  {(keyword s1) (Integer. s2)})

(defn sue-attrs
  "Parse out the attrs of a given Aunt Sue, into a map."
  [sue]
  (into {} (map (comp pairmap rest) (re-seq #"(?:(\w+): (\d+))" sue))))

(loop
  [sues (->> "resources/day16input" slurp str/split-lines)]
  (if (empty? sues)
    "End"
    (let [attrs (sue-attrs (first sues))]

      ;; Part 1
      (if (= (map analysis (keys attrs))
             (vals attrs))
        (println (first (str/split (first sues) #":")))
        (recur (rest sues))))))

(ns clojure-practice.random.ball-swap)

(comment
  " -=== Ball Swap ===-
  An illusionist is performing ball swaps.
  There are three cups on a table, at positions A, B, and C. At the start, there is a ball hidden under the cup at position B.
  Create a function that returns the letter position where the ball final position is once the swapping is finished.
  There will be several swaps perform, represented by two letters.
  For example, if I swap the cups at positions A and B, this can be represented as AB or BA.
  Examples
  swap-cups ([ AB CA ])  ➞ C

  swap-cups ([AC CA CA AC]) => B

  swap-cups ([BA AC CA BC]) => A")

#_(defn new-ball-position
    [swap-positions ball-position]
    (cond (some #(= % ball-position) swap-positions) (first (filter #(not= % ball-position) swap-positions))
          :else ball-position))

#_(defn swapped-ball-position
    [cup-positions ball-position]
    (if (some #(= % ball-position) cup-positions)
      (first (filter #(not= % ball-position)
                     cup-positions))
      ball-position))

(defn swapped-ball-position
  [[cup1 cup2] ball-cup]
  (cond (= cup1 ball-cup) cup2
        (= cup2 ball-cup) cup1
        :else ball-cup))

(defn swapped-ball-position-s
  [cups-swap-s initial-ball-position]
  (loop [[cups-swap & rem-cups-swaps] cups-swap-s
         ball-position  initial-ball-position
         ball-positions []]
    (if cups-swap
      (let [new-ball-position (swapped-ball-position cups-swap ball-position)]
        (recur rem-cups-swaps
               new-ball-position
               (conj ball-positions new-ball-position)))
      ball-positions)))

(defn swap-cups
  [swap-s]
  (let [cups-swap-s (map vec swap-s)]
    (last (swapped-ball-position-s cups-swap-s \B))))


(comment
  (def cup-swaps-input ["AB" "CA"])
  (def initial-ball-position \B)

  (swapped-ball-position (vec (first cup-swaps-input)) initial-ball-position)
  #_=> \A

  (swapped-ball-position-s cup-swaps-input \B)
  #_=> [\A \C]

  (swap-cups cup-swaps-input)
  #_=> \C

  (def input-list [["AB" "CA"]
                   ["AC" "CA" "CA" "AC"]
                   ["BA" "AC" "CA" "BC"]])

  (map #(swapped-ball-position-s % initial-ball-position) input-list)
  #_=> ([\A \C]
        [\B \B \B \B]
        [\A \C \A \A])

  (map swap-cups input-list)
  #_=> (\C \B \A)

  )

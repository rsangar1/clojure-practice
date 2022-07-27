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

(defn new-ball-position
  [swap-positions ball-position]
  (if (some #(= % ball-position) swap-positions)
    (first (filter #(not= % ball-position)
                   swap-positions))
    ball-position))

(defn ball-positions
  [cup-swaps initial-ball-position]
  (loop [[swap & rem-swaps] cup-swaps
         ball-position  initial-ball-position
         ball-positions []]
    (if swap
      (let [new-ball-position (new-ball-position (vec swap) ball-position)]
        (recur rem-swaps
               new-ball-position
               (conj ball-positions new-ball-position)))
      ball-positions)))

(defn swap-cups
  [cup-swaps]
  (-> (ball-positions cup-swaps \B)
      last))


(comment
  (def cup-swaps-input ["AB" "CA"])
  (def initial-ball-position \B)

  (new-ball-position (vec (first cup-swaps-input)) initial-ball-position)
  #_=> \A

  (ball-positions cup-swaps-input \B)
  #_=> [\A \C]

  (swap-cups cup-swaps-input)
  #_=> \C

  (def input-list [["AB" "CA"]
                   ["AC" "CA" "CA" "AC"]
                   ["BA" "AC" "CA" "BC"]])

  (map swap-cups input-list)
  #_=> (\C \B \A)

  )

(ns clojure-practice.leetcode.n-queens
  (:require [clojure-practice.advent-of-code-2020.util :as util]))

(comment
  "https://leetcode.com/problems/n-queens/")

(defn board
  [row col]
  (vec (repeat row
               (apply str
                      (repeat col ".")))))

(defn queen-exists?
  ([board row column]
   (= \Q (nth (nth board row)
              column)))
  ([board positions]
   (let [queen-positions (for [[row column] positions
                               :when (and (not (nil? row))
                                          (not (nil? column))
                                          (queen-exists? board row column))]
                           [row column])]
     ;(println "queen positions: " queen-positions)
     (> (count queen-positions) 0))))

(defn upper-positions
  "all upper positions of current position"
  [row column]
  (for [r (range row)]
    [r column]))

(defn upper-left-diagonal-positions
  "all diagonal positions on the upper left side of current position"
  [row column]
  (for [r (range row)
        c (range column)
        :when (= (- r c) (- row column))]
    [r c]))

(defn upper-right-diagonal-positions
  [row column end]
  "all diagonal positions on the upper right side of current position to the end"
  (for [r (range row)
        c (range column end)
        :when (= (+ r c) (+ row column))]
    [r c]))


(defn safe-position-for-queen?
  [board row column]
  (not (queen-exists? board
                      (concat (upper-positions row column)
                              (upper-left-diagonal-positions row column)
                              (upper-right-diagonal-positions row column (count board))))))

#_(defn complete-board?
  [board]
  (= (count (for [row (range (count board))
                  :while (not (contains? (set row) \Q))]
              row))
     0))

#_(defn safe-columns-in-row
  "safe position for next row in the board"
  [board row]
  (for [column (range (count board))
        :when (safe-position-for-queen? board row column)]
    column))

(defn place-queen
  [board row column]
  (let [current-row-value (nth board row)
        new-row-value     (apply str (util/insert-at-index (vec current-row-value)
                                                           column
                                                           \Q))]
    (util/insert-at-index board
                          row
                          new-row-value)))

(defn n-queens-solution
  [board row]
  (if (= row (count board))
    {:solution board}
    (for [column (range (count board))
          :when (safe-position-for-queen? board row column)]
      (n-queens-solution (place-queen board row column)
                         (inc row)))))

#_(defn n-queens-solution
  [board row]
  (for [row (range (count board))
        :when (= row (count board))])
  (if (= row (count board))
    (println board)
    (for [column (range (count board))
          :when (safe-position-for-queen? board row column)]
      (n-queens-solution (place-queen board row column)
                         (inc row)))))




(defn print-board [board]
  board)



(comment

  (board 4 4)
  #_=> ["...." "...." "...." "...."]

  (board 4 2)
  #_=> [".." ".." ".." ".."]

  (queen-exists? ["...Q"
                  "Q..."] 1 0)
  #_=> true

  (queen-exists? ["...Q"
                  "Q..."] 0 0)
  #_=> false

  (queen-exists? ["...Q"
                  "Q..."
                  "...."
                  "...."] [[2 2]])
  #_=> false

  (queen-exists? ["...Q"
                  "Q..."
                  "...."
                  "...."] [])
  #_=> false

  (queen-exists? ["...Q"
                  "Q..."
                  "...."
                  "...."] [[1 0] [2 2] [1 2] [0 2]])
  #_=> true


  (upper-positions 2 3)
  #_=> ([0 3] [1 3])

  (upper-positions 0 0)
  #_=> ()

  (upper-positions 2 1)
  #_=> ([0 1] [1 1])

  (upper-left-diagonal-positions 3 3)
  #_=> ([0 0] [1 1] [2 2])

  (upper-left-diagonal-positions 3 2)
  #_=> ([1 0] [2 1])

  (upper-right-diagonal-positions 2 2 5)
  #_=> ([0 4] [1 3])

  (upper-right-diagonal-positions 4 4 5)
  #_=> ()

  (safe-position-for-queen? ["...Q"
                             "Q..."
                             "...."
                             "...."] 0 0)
  #_=> true

  (safe-position-for-queen? ["...Q"
                             "Q..."
                             "...."
                             "...."] 2 1)
  #_=> false

  (safe-position-for-queen? ["...Q"
                             "Q..."
                             "...."
                             "...."] 2 2)
  #_=> true

  (safe-columns-in-row ["...Q"
                        "Q..."
                        "...."
                        "...."] 2)
  #_=> (2)

  (safe-columns-in-row ["...Q"
                        "Q..."
                        "...."
                        "...."] 3)
  #_=> (1)

  (safe-columns-in-row ["...Q"
                        "Q..."
                        "..Q."
                        "...."] 3)
  #_=> ()

  (place-queen ["...Q"
                            "Q..."
                            "...."
                            "...."] 3 3)
  #_["...Q" "Q..." "...." "...Q"]

  (upper-positions 2 0)

  (util/insert-at-index [1 2 8 4 5] 2 3)
  #_=> [1 2 3 4 5]

  (util/insert-at-index [\. \. \. \.] 2 \Q)
  #_=> [\. \. \Q \.]

  (util/insert-at-index ["...Q"
                    "Q..."
                    "...."
                    "...."] 2 "..Q.")
  #_=> ["...Q" "Q..." "..Q." "...."]


  (n-queens-solution ["...."
                      "...."
                      "...."
                      "...."] 0)

  (clojure.string/join ["...."
                              "...."
                              "...."
                              "...."])

  (time (->> (n-queens-solution (board 8 8) 0)
             flatten
             (map :solution)))

  (->> (n-queens-solution (board 4 4) 0)
       flatten
       (map :solution))
  #_=> ([".Q.." "...Q" "Q..." "..Q."] ["..Q." "Q..." "...Q" ".Q.."])

  (-> (vec "....")
       (assoc 2 \Q))

  )

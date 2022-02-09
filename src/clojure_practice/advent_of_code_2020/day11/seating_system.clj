(ns clojure-practice.advent-of-code-2020.day11.seating-system
  (:require [clojure.string :as str]))

(comment
  "https://adventofcode.com/2020/day/11"
  )

(defn input-str->data
  [input-str]
  (->> input-str
       str/split-lines))

(defn can-seat-become-occupied?
  [neighbor-seats-occupancy-map]
  (nil? (get neighbor-seats-occupancy-map \#)))

(defn can-seat-become-empty?
  [neighbor-seats-occupancy-map occupied-seats-count]
  (and (not (nil? (get neighbor-seats-occupancy-map \#)))
       (>= (get neighbor-seats-occupancy-map \#) occupied-seats-count)))

#_[(row-1, column-1) (row-1, column) (row-1, column+1)
   (row, column-1) (row, column+1)
   (row+1, column-1) (row+1, column) (row+1, column+1)]
(def neighbor-indices [[dec dec] [dec identity] [dec inc]
                       [identity dec] [identity inc]
                       [inc dec] [inc identity] [inc inc]])

(defn get-neighbor-indices
  "neighbors for a given row and column"
  [row, column]
  (map (fn [[row-fn, column-fn]]
         [(row-fn row) (column-fn column)])
       neighbor-indices))

(defn seat-status
  "get status of current seat"
  [seats [row column]]
  (let [seat-row (when (and (>= row 0)
                            (< row (count seats)))
                   (nth seats row))]
    (when (and (not (nil? seat-row))
               (>= column 0)
               (< column (count seat-row)))
      (nth seat-row column))))

(defn get-neighbor-seats-status
  "current status of all neighboring seats"
  [seats [row column]]
  (map #(seat-status seats %) (get-neighbor-indices row column)))

#_(defn adjust-seat-status
  "adjust a particular seat status based on occupancy rules"
  [seats row column]
  (let [current-seat-status      (nth (nth seats row) column)
        neighbor-seats-status    (get-neighbor-seats-status seats [row column])
        neighbor-seats-occupancy (frequencies neighbor-seats-status)]
    (cond (= current-seat-status \L) (if (can-seat-become-occupied? neighbor-seats-occupancy)
                                       \#
                                       \L)
          (= current-seat-status \#) (if (can-seat-become-empty? neighbor-seats-occupancy 4)
                                       \L
                                       \#)
          (= current-seat-status \.) \.
          :else current-seat-status)))

(defn adjust-seat-row-status
  "adjust seat status of current row"
  [seats row]
  (let [current-row (nth seats row)]
    (loop [column      0
           updated-row []]
      (if (= column (count current-row))
        (apply str updated-row)
        (recur (inc column)
               (conj updated-row (adjust-seat-status seats row column)))))))

(defn adjust-seats-status
  "adjust status of all seats"
  [seats]
  (loop [row           0
         updated-seats []]
    (if (= row (count seats))
      updated-seats
      (recur (inc row)
             (conj updated-seats (adjust-seat-row-status seats row))))))

(defn ^:private count-seats-by-occupancy
  [seats]
  (->> seats
       (map frequencies)
       (apply merge-with +)))

(defn find-stable-seating-layout
  "adjust seats status until a stable layout is obtained"
  [seats]
  (loop [current-seats-layout seats
         new-seats-layout     (adjust-seats-status current-seats-layout)]
    ;(println "current-seats-layout: " current-seats-layout)
    ;(println "new-seats-layout: " new-seats-layout)
    (if (= new-seats-layout current-seats-layout)
      new-seats-layout
      (recur new-seats-layout
             (adjust-seats-status new-seats-layout)))))

(defn part1
  [input-file-path]
  (let [input-str             (slurp input-file-path)
        input-data            (input-str->data input-str)

        ;_                     (println "input data: " input-data)
        stable-seating-layout (find-stable-seating-layout input-data)
        final-occupancy-count (count-seats-by-occupancy stable-seating-layout)
        ;_                     (println "final occupancy layout" final-occupancy-count)
        ]
    (get final-occupancy-count \#)))


;;;;;;;; PART-2 ;;;;;;;;;

(defn find-first-neighbor-seat
  "find first seat from given seat using the row-fn and column-fn"
  [seats [row column] [row-fn column-fn]]
  (loop [current-row    (row-fn row)
         current-column (column-fn column)
         seat           nil]
    ;(println "current-row:" current-row "current-column:" current-column "seat:" seat)
    (if (or (and (not (nil? seat)) (not= seat \.))
            (< current-row 0)
            (>= current-row (count seats))
            (< current-column 0)
            (>= current-column (count (nth seats row))))
      seat
      (recur (row-fn current-row)
             (column-fn current-column)
             (seat-status seats [current-row current-column])))))

(defn find-all-first-neighbor-seats
  [seats [row column]]
  (map #(find-first-neighbor-seat seats [row column] %) neighbor-indices))

(defn adjust-seat-status
  "adjust a particular seat status based on occupancy rules"
  [seats row column]
  (let [current-seat-status      (nth (nth seats row) column)
        neighbor-seats-status    (find-all-first-neighbor-seats seats [row column])
        neighbor-seats-occupancy (frequencies neighbor-seats-status)]
    (cond (= current-seat-status \L) (if (can-seat-become-occupied? neighbor-seats-occupancy)
                                       \#
                                       \L)
          (= current-seat-status \#) (if (can-seat-become-empty? neighbor-seats-occupancy 5)
                                       \L
                                       \#)
          (= current-seat-status \.) \.
          :else current-seat-status)))


(defn part2
  [input-file-path]
  (let [input-str             (slurp input-file-path)
        input-data            (input-str->data input-str)

        ;_                     (println "input data: " input-data)
        stable-seating-layout (find-stable-seating-layout input-data)
        final-occupancy-count (count-seats-by-occupancy stable-seating-layout)
        ;_                     (println "final occupancy layout" final-occupancy-count)
        ]
    (get final-occupancy-count \#)))


(comment
  (input-str->data (slurp "resources/day11/sample-input.txt"))
  #_=> ["L.LL.LL.LL"
        "LLLLLLL.LL"
        "L.L.L..L.."
        "LLLL.LL.LL"
        "L.LL.LL.LL"
        "L.LLLLL.LL"
        "..L.L....."
        "LLLLLLLLLL"
        "L.LLLLLL.L"
        "L.LLLLL.LL"]

  (do
    (def input-data (input-str->data (slurp "resources/day11/sample-input.txt")))
    (println input-data))

  (get-neighbor-indices 3 4)
  #_=> [[2 3] [2 4] [2 5] [3 3] [3 5] [4 3] [4 4] [4 5]]

  (get-neighbor-indices 0 0)
  #_=> [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]

  (seat-status input-data [2 3])
  #_=> \.

  (get-neighbor-seats-status input-data [2 3])
  #_=> (\L \L \L \L \L \L \L \.)

  (get-neighbor-seats-status input-data [0 0])
  #_=> (nil nil nil nil \. nil \L \L)

  (adjust-seat-status input-data 0 0)
  #_=> \#

  (adjust-seat-row-status input-data 1)
  #_=> "#######.##"

  (adjust-seats-status input-data)
  #_=> ["#.##.##.##"
        "#######.##"
        "#.#.#..#.."
        "####.##.##"
        "#.##.##.##"
        "#.#####.##"
        "..#.#....."
        "##########"
        "#.######.#"
        "#.#####.##"]

  (find-stable-seating-layout input-data)
  #_=> ["#.#L.L#.##"
        "#LLL#LL.L#"
        "L.#.L..#.."
        "#L##.##.L#"
        "#.#L.LL.LL"
        "#.#L#L#.##"
        "..L.L....."
        "#L#L##L#L#"
        "#.LLLLLL.L"
        "#.#L#L#.##"]

  (count-seats-by-occupancy input-data)
  #_=> {\L 71, \. 29}

  (count-seats-by-occupancy (find-stable-seating-layout input-data))
  #_=> {\# 37, \. 29, \L 34}

  (part1 "resources/day11/sample-input.txt")
  #_=> 37

  (part1 "resources/day11/input.txt")
  #_=> 2263


  ;;; PART-2 ;;;
  (find-first-neighbor-seat [".......#."
                             "...#....."
                             ".#......."
                             "........."
                             "..#L....#"
                             "....#...."
                             "........."
                             "#........"
                             "...#....."] [4 3] [dec identity])
  #_=> \#

  (find-first-neighbor-seat ["............."
                             ".L.L.#.#.#.#."
                             "............."] [1 1] [identity inc])
  #_=> \L

  (find-all-first-neighbor-seats input-data [3 3])
  #_=> (\L \L \L \L \L \L \L \L)

  (find-all-first-neighbor-seats [".......#."
                                  "...#....."
                                  ".#......."
                                  "........."
                                  "..#L....#"
                                  "....#...."
                                  "........."
                                  "#........"
                                  "...#....."] [4 3])
  #_=> (\# \# \# \# \# \# \# \#)

  (find-all-first-neighbor-seats ["............."
                                  ".L.L.#.#.#.#."
                                  "............."] [1 1])
  #_=> (\. \. \. \. \L \. \. \.)

  (adjust-seat-status input-data 1 2)
  #_=> \#

  (part2 "resources/day11/sample-input.txt")
  #_=> 26

  (part2 "resources/day11/input.txt")
  #_=> 2002


  )

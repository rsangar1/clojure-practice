(ns clojure-practice.advent-of-code-2020.day11.seating-system
  (:require [clojure.string :as str]))

(comment
  "https://adventofcode.com/2020/day/11"
  )

(defn input-str->data
  [input-str]
  (->> input-str
       str/split-lines))


(defn ^:private count-seats-by-occupancy
  [seats]
  (->> seats
       (map frequencies)
       (apply merge-with +)))

(defn can-seat-become-occupied?
  [neighbor-seats-occupancy-map]
  (nil? (get neighbor-seats-occupancy-map \#)))

(defn can-seat-become-empty?
  [neighbor-seats-occupancy-map occupied-seats-count]
  (and (not (nil? (get neighbor-seats-occupancy-map \#)))
       (>= (get neighbor-seats-occupancy-map \#) occupied-seats-count)))

(defn get-neighbor-indices
  "neighbors for a given row and column
  [(row-1, column-1) (row-1, column) (row-1, column+1)
   (row, column-1)                   (row, column+1)
   (row+1, column-1) (row+1, column) (row+1, column+1)]"
  [row, column]
  [[(dec row) (dec column)] [(dec row) column] [(dec row) (inc column)]
   [row (dec column)] [row, (inc column)]
   [(inc row) (dec column)] [(inc row) column] [(inc row) (inc column)]])

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

(defn adjust-seat-status
  "adjust a particular seat status based on occupancy rules"
  [seats row column]
  (let [current-seat-status (nth (nth seats row) column)
        neighbor-seats-status (get-neighbor-seats-status seats [row column])
        neighbor-seats-occupany (frequencies neighbor-seats-status)]
    (cond (= current-seat-status \L) (if (can-seat-become-occupied? neighbor-seats-occupany)
                                       \#
                                       \L)
          (= current-seat-status \#) (if (can-seat-become-empty? neighbor-seats-occupany 4)
                                      \L
                                      \#)
          (=  current-seat-status \.) \.
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
  (loop [row 0
         updated-seats []]
    (if (= row (count seats))
      updated-seats
      (recur (inc row)
             (conj updated-seats (adjust-seat-row-status seats row))))))

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
        grid                  (build-grid input-data)
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

  (get-neighbor-indices 0 0)
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
  #_ => ["#.##.##.##"
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

  (part1 "resources/day11/sample-input.txt")
  #_=> 37

  (part1 "resources/day11/input.txt")
  #_=> 2263

  )

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
  [adj-seats-occupancy-map]
  (nil? (get adj-seats-occupancy-map \#)))

(defn can-seat-become-empty?
  [adj-seats-occupancy-map]
  (>= (get adj-seats-occupancy-map \#) 5))

(defn change-current-seat-state
  [index previous-row current-row next-row]
  (let [begin-index            (dec index)
        end-index              (+ index 2)
        ;_                      (println begin-index end-index previous-row-adj-seats (subs current-row begin-index end-index) next-row-adj-seats)
        adj-seats-occupancy    (count-seats-by-occupancy [(subs previous-row begin-index end-index)
                                                          (subs current-row begin-index end-index)
                                                          (subs next-row begin-index end-index)])
        ;_                      (println adj-seats-occupancy)
        ]
    (cond (= \L (nth current-row index)) (if (can-seat-become-occupied? adj-seats-occupancy)
                                           \#
                                           \L)
          (= \# (nth current-row index)) (if (can-seat-become-empty? adj-seats-occupancy)
                                           \L
                                           \#)
          (= \. (nth current-row index)) \.
          :else (nth current-row index))))

(defn adjust-seat-layout*
  [previous-row current-row next-row]
  (loop [index  1
         result [" "]]
    (if (= index (dec (count current-row)))
      (apply str (conj result " "))
      (recur (inc index)
             (conj result (change-current-seat-state index previous-row current-row next-row))))))

(defn adjust-seat-layout
  [seat-rows]
  (loop [index        1
         previous-row (nth seat-rows (dec index))
         current-row  (nth seat-rows index)
         next-row     (nth seat-rows (inc index))
         result       [(first seat-rows)]]
    ;(println index (count seat-rows) previous-row current-row next-row)
    ;(println "after re-assigning each index =" index "row =" current-row "result =" result)
    (if (= index (- (count seat-rows) 2))
      (conj (conj result (adjust-seat-layout* previous-row current-row next-row)) (last seat-rows))
      (recur (inc index)
             current-row
             next-row
             (nth seat-rows (+ index 2))
             (conj result (adjust-seat-layout* previous-row current-row next-row))))))

(defn find-stable-seating-layout
  [input-data]
  (loop [current-seats-layout input-data
         new-seats-layout     (adjust-seat-layout current-seats-layout)]
    ;(println "current-seats-layout: " current-seats-layout)
    ;(println "new-seats-layout: " new-seats-layout)
    (if (= new-seats-layout current-seats-layout)
      new-seats-layout
      (recur new-seats-layout
             (adjust-seat-layout new-seats-layout)))))

(defn build-grid
  [input-data]
  (let [input     (->> input-data
                       (mapv #(str " " % " ")))
        row-len   (count (first input))
        empty-row (apply str (repeat row-len " "))]
    (loop [index  0
           result [empty-row]]
      (if (= index (count input))
        (conj result empty-row)
        (recur (inc index)
               (conj result (nth input index)))))))

(defn part1
  [input-file-path]
  (let [input-str             (slurp input-file-path)
        input-data            (input-str->data input-str)
        grid                  (build-grid input-data)
        ;_                     (println "grid: " grid)
        ;_                     (println "input data: " input-data)
        stable-seating-layout (find-stable-seating-layout grid)
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

  (map frequencies [".LL" "LLL" ".L."])
  (apply merge-with + [{\. 1, \L 2} {\L 3} {\. 2, \L 1}])
  (merge-with + {\. 1, \L 2} {\L 3} {\. 2, \L 1})
  (reduce #(merge-with +) {} [{\. 1, \L 2} {\L 3} {\. 2, \L 1}])

  (map frequencies (apply vec [(vec "L.LL.LL.LL") (vec "LLLLLLL.LL") (vec "L.L.L..L..")]))

  (map frequencies ["L.LL.LL.LL" "LLLLLLL.LL" "L.L.L..L.."])

  (nth "LLLLLLL.LL" 5)

  (vec "LLLLLLL.LL")
  (subs ".LL" 0 1)

  (= [".LL" "LLL"] [".LL" "LLL"])

  (count-seats-by-occupancy [".LL" "L#L" ".L."])

  (change-current-seat-state 2 "L.LL.LL.LL" "LL#LLLL.LL" "L.L.L..L..")
  #_=> \#

  (adjust-seat-layout input-data)
  #_=> ["L.LL.LL.LL" "LLLLLLL.LL" "L.L.L..L.." "LLLL.LL.LL" "L.LL.LL.LL" "L.LLLLL.LL" "..L.L....." "LLLLLLLLLL" "L.LLLLLL.L"]


  (adjust-seat-layout* "" "L.LL.LL.LL" "LLLLLLL.LL")
  #_=> "#.##.##.##"
  (adjust-seat-layout* "" "#.##.##.##" "#######.##")
  #_=> "#.LL.L#.##"
  (adjust-seat-layout* "" "#.LL.L#.##" "#LLLLLL.L#")
  #_=> "#.##.L#.##"
  (adjust-seat-layout* "" "#.##.L#.##" "#L###LL.L#")
  #_=> "#.#L.L#.##"



  (adjust-seat-layout* "..L.L....." "LLLLLLLLLL" "L.LLLLLL.L")
  #_=> "##########"
  (adjust-seat-layout* "..#.#....." "##########" "#.######.#")
  #_=> "#LLLLLLLL#"
  (adjust-seat-layout* "..L.L....." "#LLLLLLLL#" "#.LLLLLL.L")
  #_=> "#L######L#"
  (adjust-seat-layout* "..#.#....." "#L######L#" "#.LL###L.L")
  #_=> "#L#LLLL#L#"
  (adjust-seat-layout* "..L.L....." "#L#LLLL#L#" "#.LLLLLL.L")
  #_=> "#L#L##L#L#"

  (adjust-seat-layout ["L.LL.LL.LL"
                       "LLLLLLL.LL"
                       "L.L.L..L.."
                       "LLLL.LL.LL"
                       "L.LL.LL.LL"
                       "L.LLLLL.LL"
                       "..L.L....."
                       "LLLLLLLLLL"
                       "L.LLLLLL.L"
                       "L.LLLLL.LL"])
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

  (time (part1 "resources/day11/sample-input.txt"))
  #_=> 37

  (= ["#.#L.L#.##" "#LLL#LL.L#" "L.#.L..#.." "#L##.##.L#" "#.#L.LL.LL" "#.#L#L#.##" "..L.L....." "#L#L##L#L#" "#.LLLLLL.L" "#.#L#L#.##"]
     ["#.#L.L#.##" "#LLL#LL.L#" "L.#.L..#.." "#L##.##.L#" "#.#L.LL.LL" "#.#L#L#.##" "..L.L....." "#L#L##L#L#" "#.LLLLLL.L" "#.#L#L#.##"])

  (time (part1 "resources/day11/input.txt"))
  #_=> 2263

  (input-str->data (slurp "resources/day11/input.txt"))

  )

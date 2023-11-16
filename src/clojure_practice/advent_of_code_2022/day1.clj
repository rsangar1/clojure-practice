(ns clojure-practice.advent-of-code-2022.day1
  (:require [clojure-practice.util :as util]
            [clojure.string :as str]))

(comment
  "https://adventofcode.com/2022/day/1"
  )

(defn input-file->calories-data
  [input-file-path]
  (let [input-str (util/read-file input-file-path)]
    (->> (str/split input-str #"\n\n")
         (map str/split-lines)
         (map #(map read-string %)))))

(defn total-calories-by-elf
  [calories-data]
  (->> calories-data
       (map #(apply + %))))

(defn part-1
  [input-file-path]
  (let [calories-data (input-file->calories-data input-file-path)]
    (->> calories-data
         (total-calories-by-elf)
         (apply max))))

(defn part-2
  [input-file-path]
  (let [calories-data (input-file->calories-data input-file-path)
        top-n 3]
    (->> calories-data
         (total-calories-by-elf)
         (sort >)
         (take top-n)
         (apply +))))

(comment

  (input-file->calories-data "resources/advent_of_code_2022/day1/sample_input.txt")
  #_=> ((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000))

  (total-calories-by-elf '((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000)))
  #_=> (6000 4000 11000 24000 10000)



  (part-1 "resources/advent_of_code_2022/day1/sample_input.txt")
  #_=> 24000

  (input-file->calories-data "resources/advent_of_code_2022/day1/puzzle_input.txt")

  (part-1 "resources/advent_of_code_2022/day1/puzzle_input.txt")
  #_=> 75622

  (part-2 "resources/advent_of_code_2022/day1/puzzle_input.txt")
  #_=> 213159

  (def input "3276,15213804437,20059303617,22532713944,34086783151,41500058078,46501036896,76950450424,80187824590,196027211618,200098000009,200098000009,200980000001,299062000008,299509000004,299563000002,299603000009,299603000009,299609000003,681131086172,710425497728,710425573613,789374540410,817096014640,8720077180468,8809859236606,9781557481054")
  (let [split-list (str/split input #",")]
    (map ))

  )


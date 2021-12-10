(ns clojure-practice.advent-of-code-2020.day9.encoding-error
  (:require [clojure.string :as str]))

(comment
  "
  --- Day 9: Encoding Error ---
  With your neighbor happily enjoying their video game, you turn your attention to an open data port on the little screen
  in the seat in front of you.

  Though the port is non-standard, you manage to connect it to your computer through the clever use of several paperclips.
  Upon connection, the port outputs a series of numbers (your puzzle input) .

  The data appears to be encrypted with the eXchange-Masking Addition System (XMAS) which, conveniently for you, is an old
  cypher with an important weakness.

  XMAS starts by transmitting a preamble of 25 numbers. After that, each number you receive should be the sum of any two
  of the 25 immediately previous numbers. The two numbers will have different values, and there might be more than one such pair.

  For example, suppose your preamble consists of the numbers 1 through 25 in a random order. To be valid, the next number
  must be the sum of two of those numbers:

  26 would be a valid next number, as it could be 1 plus 25 (or many other pairs, like 2 and 24) .
  49 would be a valid next number, as it is the sum of 24 and 25.
  100 would not be valid                                   ; no two of the previous 25 numbers sum to 100.
  50 would also not be valid                               ; although 25 appears in the previous 25 numbers, the two numbers in the pair must be different.
  Suppose the 26th number is 45, and the first number (no longer an option, as it is more than 25 numbers ago) was 20. Now,
  for the next number to be valid, there needs to be some pair of numbers among 1-19, 21-25, or 45 that add up to it:

  26 would still be a valid next number, as 1 and 25 are still within the previous 25 numbers.
  65 would not be valid, as no two of the available numbers sum to it.
  64 and 66 would both be valid, as they are the result of 19+45 and 21+45 respectively.
  Here is a larger example which only considers the previous 5 numbers (and has a preamble of length 5) :

  35
  20
  15
  25
  47
  40
  62
  55
  65
  95
  102
  117
  150
  182
  127
  219
  299
  277
  309
  576
  In this example, after the 5-number preamble, almost every number is the sum of two of the previous 5 numbers ; the
  only number that does not follow this rule is 127.

  The first step of attacking the weakness in the XMAS data is to find the first number in the list (after the preamble)
  which is not the sum of two of the 25 numbers before it. What is the first number that does not have this property?")


(defn input-str->encrypted-data
  [input-str]
  (->> input-str
       str/split-lines
       (map #(Long/parseLong %))
       vec))

(defn any-two-sum-equals-target?
  "check if sum of any of the two numbers in v equals to given target"
  [v target]
  (not (every? false? (map #(and (not (nil? %))
                                 (not= % (- target %))
                                 (contains? (set v) (- target %))) v))))

(defn first-invalid-number
  "finds the first invalid number based on given preamble number of elements"
  [input preamble]
  (loop [i      input
         v      (take preamble input)
         target (nth i preamble)]
    (if (and (= preamble (count v))
             (any-two-sum-equals-target? v target))
      (recur (next i)
             (take preamble i)
             (nth i preamble))
      target)))

(defn part1
  [input-file-path]
  (let [input-str  (slurp input-file-path)
        input-data (input-str->encrypted-data input-str)
        preamble   25]
    (first-invalid-number input-data preamble)))






(defn find-contiguous-subvec
  "finds contiguous elements in given vec v starting with index i that sums up to target"
  [v target i]
  (loop [v1  v
         j   i
         sum 0]
    (if (and (not (nil? v1))
             (< sum target))
      (recur (next v1)
             (inc j)
             (+ sum (nth v j)))
      (when (= sum target)
        (subvec v i j)))))

(defn ^:private encryption-weakness!
  "sum of max and min elements in given vec v"
  [v]
  (+ (apply min v)
     (apply max v)))

(defn encryption-weakness
  "finds encryption weakness in given vec v and target"
  [v target]
  (loop [i 0]
    (let [res (find-contiguous-subvec v target i)]
      (if (and (> (count v) i)
               (not (nil? res)))
        (encryption-weakness! res)
        (recur (inc i))))))

(defn part2
  [input-file-path]
  (let [input-str  (slurp input-file-path)
        input-data (input-str->encrypted-data input-str)
        target     (part1 input-file-path)]
    (encryption-weakness input-data target)))



(comment

  ;;; part-1
  (input-str->encrypted-data (slurp "resources/day9/sample-input.txt"))
  #_=> [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576]

  (any-two-sum-equals-target? [2 3 4] 8)
  #_=> false

  (any-two-sum-equals-target? [2 3 4] 6)

  (loop-recur-ex [2 3 4 5 6])

  (any-two-sum-equals-target? [2 3 2 1] 10)

  (first-invalid-number [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576] 5)
  #_=> 127

  (part1 "resources/day9/aoc-input.txt")
  #_=> 258585477




  ;;; part-2

  (find-contiguous-subvec [2 3 4 5] 9 0)
  #_=> [2 3 4]

  (encryption-weakness! [2 3 4 5])
  #_=> 7

  (encryption-weakness! [2 3 4 5 99])
  #_=> 101

  (encryption-weakness [2 3 4 5] 9)
  #_=> 6

  (encryption-weakness [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576] 127)
  #_=> 62

  (part2 "resources/day9/aoc-input.txt")
  #_=> 36981213

  )

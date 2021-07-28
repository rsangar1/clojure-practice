(ns clojure-practice.advent-of-code-2020.day7.handy-haversacks
  (:require [clojure.string :as str]
            [clojure-practice.advent-of-code-2020.util :as util]))

(comment
  "
  --- Day 7: Handy Haversacks ---
  You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab
  some food: all flights are currently delayed due to issues in luggage processing.

  Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents ;
  bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody
  responsible for these regulations considered how long they would take to enforce!

  For example, consider the following rules:

  light red bags contain 1 bright white bag, 2 muted yellow bags.
  dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  bright white bags contain 1 shiny gold bag.
  muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.
  dotted black bags contain no other bags.

  These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every
  vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.

  You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be
  valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)

  In the above rules, the following options would be available to you:

  A bright white bag, which can hold your shiny gold bag directly.
  A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
  A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
  A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
  So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.

  How many bag colors can eventually contain at least one shiny gold bag?
  (The list of rules is quite long; make sure you get all of it.)"
  )

#_(defn content-str->content-entry
    [content]
    (if (not (= content "no other bags."))
      (let [[needed-content _] (str/split content #" bag")
            [count texture color] (str/split needed-content #" ")]
        {:type  (str texture " " color)
         :count (Integer/parseInt count)})
      {}))

#_(defn input-line->bag-entry
    "convert given input line to a map entry"
    [input-line]
    (let [[bag contents] (str/split input-line #" bags contain ")
          bag-content-entries (map content-str->content-entry (str/split contents #", "))]
      {bag (vec bag-content-entries)}))

#_(defn input-str->bags-data
    "convert given input string to bags map"
    [input-str]
    (->> input-str
         (str/split-lines)
         (map input-line->bag-entry)
         (apply merge)))

(defn content-str->content
  [content-str]
  (if (not (= content-str "no other bags."))
    (let [[needed-content _] (str/split content-str #" bag")
          [count texture color] (str/split needed-content #" ")]
      [(Integer/parseInt count) (str texture " " color)])
    [0 ""]))

(defn input-line->bag-entry
  "convert given input line to a map entry"
  [input-line]
  (let [[bag-color contents] (str/split input-line #" bags contain ")
        bag-contents (map content-str->content (str/split contents #", "))]
    (when some? bag-contents
      {bag-color (vec bag-contents)})))

(defn input-str->bags-data
  "convert given input string to bags map"
  [input-str]
  (->> input-str
       (str/split-lines)
       (map input-line->bag-entry)
       (apply merge)))

#_(defn contain-colored-bag-directly?
  "if color exists directly in the current bag"
  [bag color]
  (some (fn [[_ c]]
          (= c color)) bag))

#_(defn contains-colored-bag?
  "check if given colored bag exists somewhere within the bag.
  if nil, return false"
  [bags bag-content color]
  (or (contain-colored-bag-directly? bag-content color)
      (some true?
            (map #(contains-colored-bag? bags
                                        (bags (second %))
                                        color)
                 bag-content))
      false))

(defn contains-colored-bag?
  [bags-data current-bag-color search-color]                     ;;TODO names could be better
  (->> (bags-data current-bag-color)
       (some (fn [[_ color]]
               (or (= color search-color)
                   (contains-colored-bag? bags-data color search-color)
                   false)))))

(defn bags-holding-colored-bag
  "count bags that hold at least one of the given bag color directly or within"
  [bags-data color]
  (let [bag-colors (keys bags-data)]
    (->> bag-colors
         (map #(contains-colored-bag? bags-data % color))
         (filter true?)
         count)))

(defn part1
  [input-file-path]
  (let [bags-data (input-str->bags-data (util/read-input-file input-file-path))]
    (bags-holding-colored-bag bags-data "shiny gold")))

(defn count-bags-in-a-bag
  [bags-data bag-color]
  (->> (bags-data bag-color)
       (reduce (fn [result [n color]]
                 (+ result
                    n
                    (* n (count-bags-in-a-bag bags-data color))))
               0)))

(defn part2
  [input-file-path]
  (let [bags-data (input-str->bags-data (util/read-input-file input-file-path))]
    (count-bags-in-a-bag bags-data "shiny gold")))




(comment
  ;(content-str->content-entry "1 bright white bag")
  ;#_=> {:type "bright white", :count 1}
  ;
  ;(content-str->content-entry "2 muted yellow bags.")
  ;#_=> {:type "muted yellow", :count 2}
  ;
  ;(content-str->content-entry "1 shiny gold bag.")
  ;#_=> {:type "shiny gold", :count 1}
  ;
  ;(content-str->content-entry "no other bags.")
  ;#_=> {}

  (content-str->content "1 bright white bag")
  #_=> [1 "bright white"]

  (content-str->content "2 muted yellow bags.")
  #_=> [2 "muted yellow"]

  (content-str->content "1 shiny gold bag.")
  #_=> [1 "shiny gold"]

  (content-str->content "no other bags.")
  #_=> nil

  ;(input-line->bag-entry "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  ;#_=> {"light red" [{:type "bright white", :count 1} {:type "muted yellow", :count 2}]}
  ;
  ;(input-line->bag-entry "bright white bags contain 1 shiny gold bag.")
  ;#_=> {"bright white" [{:type "shiny gold", :count 1}]}
  ;
  ;(input-line->bag-entry "dotted black bags contain no other bags.")
  ;#_=> {"dotted black" [{}]}

  (input-line->bag-entry "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  #_=> {"light red" [[1 "bright white"] [2 "muted yellow"]]}

  (input-line->bag-entry "bright white bags contain 1 shiny gold bag.")
  #_=> {"bright white" [[1 "shiny gold"]]}

  (input-line->bag-entry "dotted black bags contain no other bags.")
  #_=> {"dotted black" [[]]}

  ;(input-str->bags-data (slurp "resources/day7/sample-input.txt"))
  ;#_=> {"muted yellow" [{:type "shiny gold", :count 2} {:type "faded blue", :count 9}],
  ;      "light red"    [{:type "bright white", :count 1} {:type "muted yellow", :count 2}],
  ;      "dotted black" [{}],
  ;      "dark orange"  [{:type "bright white", :count 3} {:type "muted yellow", :count 4}],
  ;      "bright white" [{:type "shiny gold", :count 1}],
  ;      "shiny gold"   [{:type "dark olive", :count 1} {:type "vibrant plum", :count 2}],
  ;      "faded blue"   [{}],
  ;      "vibrant plum" [{:type "faded blue", :count 5} {:type "dotted black", :count 6}],
  ;      "dark olive"   [{:type "faded blue", :count 3} {:type "dotted black", :count 4}]}

  (input-str->bags-data (slurp "resources/day7/sample-input.txt"))
  #_{"muted yellow" [[2 "shiny gold"] [9 "faded blue"]],
     "light red"    [[1 "bright white"] [2 "muted yellow"]],
     "dotted black" [[]],
     "dark orange"  [[3 "bright white"] [4 "muted yellow"]],
     "bright white" [[1 "shiny gold"]],
     "shiny gold"   [[1 "dark olive"] [2 "vibrant plum"]],
     "faded blue"   [[]],
     "vibrant plum" [[5 "faded blue"] [6 "dotted black"]],
     "dark olive"   [[3 "faded blue"] [4 "dotted black"]]}

  (def sample-data {"muted yellow" [[2 "shiny gold"] [9 "faded blue"]],
                    "light red"    [[1 "bright white"] [2 "muted yellow"]],
                    "dotted black" [[0 ""]],
                    "dark orange"  [[3 "bright white"] [4 "muted yellow"]],
                    "bright white" [[1 "shiny gold"]],
                    "shiny gold"   [[1 "dark olive"] [2 "vibrant plum"]],
                    "faded blue" [[0 ""]],
                    "vibrant plum" [[5 "faded blue"] [6 "dotted black"]],
                    "dark olive"   [[3 "faded blue"] [4 "dotted black"]]})

  ;(contain-colored-bag-directly? [[2 "shiny gold"] [9 "faded blue"]] "shiny gold")
  ;#_=> true
  ;
  ;(contain-colored-bag-directly? [[3 "bright white"] [4 "muted yellow"]] "shiny gold")
  ;#_=> nil

  ;(contain-colored-bag-directly? [[]] "shiny gold")
  ;#_=> nil
  ;
  ;(contains-colored-bag? sample-data [[3 "bright white"] [4 "muted yellow"]] "shiny gold")
  ;#_=> true
  ;
  ;(contains-colored-bag? sample-data [[]] "shiny gold")
  ;#_=> false

  (contains-colored-bag? sample-data "muted yellow" "shiny gold")
  #_=> true

  (contains-colored-bag? sample-data "dark orange" "shiny gold")
  #_=> true

  (contains-colored-bag? sample-data "vibrant plum" "shiny gold")
  #_=> nil

  (bags-holding-colored-bag sample-data "shiny gold")
  #_=> 4

  (part1 "resources/day7/aoc-input1.txt")
  #_=> 177



  (count-bags-in-a-bag sample-data "shiny gold")
  #_=> 32

  (part2 "resources/day7/sample-input-part2.txt")
  #_=> 126

  (part2 "resources/day7/aoc-input1.txt")
  #_=> 34988

  )

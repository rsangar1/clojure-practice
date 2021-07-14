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

(defn content-str->content-entry
  [content]
  (if (not (= content "no other bags."))
    (let [[needed-content _] (str/split content #" bag")
          [count texture color] (str/split needed-content #" ")]
      {:type  (str texture " " color)
       :count (Integer/parseInt count)})
    {}))

(defn input-line->bag-entry
  [input-line]
  (let [[bag contents] (str/split input-line #" bags contain ")
        ;[content-bag1 content-bag2] (str/split contents #", ")
        bag-content-entries (map content-str->content-entry (str/split contents #", "))]
    {bag (vec bag-content-entries)}))

(defn input-str->bags-data
  "convert given input string to bags map"
  [input-str]
  (->> input-str
    (str/split-lines)
    (map input-line->bag-entry)
    (apply merge)))

#_(defn contain-shiny-gold-bag?
  [bag-contents]
  (-> (map #(= "shiny gold" (:type %)) bag-contents)
    set
    (contains? true)))

(defn contain-bag-type-directly?
  [bag-content bag-type]
  (some true? (map #(= bag-type (:type %)) bag-content)))

(defn contain-bag-type?
  [bags bag-content bag-type]
  (or (contain-bag-type-directly? bag-content bag-type)
    (some true? (map #(contain-bag-type? bags (bags (:type %)) bag-type) bag-content))
    false))

(defn count-bags-with-bag-type
  [bags bag-type]
  (let [bag-types (keys bags)]
    (->> bag-types
      (map #(contain-bag-type? bags (bags %) bag-type))
      (filter true?)
      count)))

(defn part1
  [input-file-path]
  (let [bags-data (input-str->bags-data (util/read-input-file input-file-path))]
    (count-bags-with-bag-type bags-data "shiny gold")))

#_(defn contain-bag-type?
  [bags bag-type]
  (->> bags
    (some )))

#_(defn count-shiny-gold-bags
  [bag-contents]
  (loop [x bag-contents
         count 0]
    (if ())))

#_(defn count-shiny-gold-bags
  "count shiny gold bags in a given bag"
  [bag-contents]
  (loop [x bag-contents
         c 0]
    (if (contain-shiny-gold-bag? x)
      (+ c (:count x))
      0)
    (recur )
    )
  )

;(defn contains-shiny-gold-bag
;  "number of bag colors that can eventually contain at least one shiny gold bag"
;  [input-file-path]
;
;  )


(comment
  (content-str->content-entry "1 bright white bag")
  #_=> {:type "bright white", :count 1}

  (content-str->content-entry "2 muted yellow bags.")
  #_=> {:type "muted yellow", :count 2}

  (content-str->content-entry "1 shiny gold bag.")
  #_=> {:type "shiny gold", :count 1}

  (content-str->content-entry "no other bags.")
  #_=> {}

  (input-line->bag-entry "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  #_=> {"light red" [{:type "bright white", :count 1} {:type "muted yellow", :count 2}]}

  (input-line->bag-entry "bright white bags contain 1 shiny gold bag.")
  #_=> {"bright white" [{:type "shiny gold", :count 1}]}

  (input-line->bag-entry "dotted black bags contain no other bags.")
  #_=> {"dotted black" [{}]}

  (input-str->bags-data (slurp "resources/day7/sample-input.txt"))
  #_=> {"muted yellow" [{:type "shiny gold", :count 2} {:type "faded blue", :count 9}],
        "light red"    [{:type "bright white", :count 1} {:type "muted yellow", :count 2}],
        "dotted black" [{}],
        "dark orange"  [{:type "bright white", :count 3} {:type "muted yellow", :count 4}],
        "bright white" [{:type "shiny gold", :count 1}],
        "shiny gold"   [{:type "dark olive", :count 1} {:type "vibrant plum", :count 2}],
        "faded blue"   [{}],
        "vibrant plum" [{:type "faded blue", :count 5} {:type "dotted black", :count 6}],
        "dark olive"   [{:type "faded blue", :count 3} {:type "dotted black", :count 4}]}

  (def sample-data {"muted yellow" [{:type "shiny gold", :count 2} {:type "faded blue", :count 9}],
                    "light red"    [{:type "bright white", :count 1} {:type "muted yellow", :count 2}],
                    "dotted black" [{}],
                    "dark orange"  [{:type "bright white", :count 3} {:type "muted yellow", :count 4}],
                    "bright white" [{:type "shiny gold", :count 1}],
                    "shiny gold"   [{:type "dark olive", :count 1} {:type "vibrant plum", :count 2}],
                    "faded blue"   [{}],
                    "vibrant plum" [{:type "faded blue", :count 5} {:type "dotted black", :count 6}],
                    "dark olive"   [{:type "faded blue", :count 3} {:type "dotted black", :count 4}]})

  (contain-bag-type-directly? [{:type "shiny gold", :count 1}] "shiny gold")
  #_#_=> true

  (contain-bag-type-directly? [{:type "bright white", :count 3} {:type "muted yellow", :count 4}] "shiny gold")
  #_=> nil
  ;(some true? ["a"])
  ;(map #(= "shiny gold" (:type %)) [{:type "bright white", :count 3} {:type "muted yellow", :count 4}])

  (contain-bag-type-directly? [{}] "shiny gold")
  #_=> nil

  (contain-bag-type? sample-data [{:type "bright white", :count 3} {:type "muted yellow", :count 4}] "shiny gold")
  #_=> true

  (contain-bag-type? sample-data [{}] "shiny gold")
  #_=> false

  (count-bags-with-bag-type sample-data "shiny gold")

  ;(contain-shiny-gold-bag? [{:type "shiny gold", :count 2} {:type "faded blue", :count 9}])
  ;#_=> true
  ;
  ;(contain-shiny-gold-bag? [{:type "bright white", :count 1} {:type "muted yellow", :count 2}])
  ;#_=> false
  ;
  ;(contain-shiny-gold-bag? [{:type "shiny gold", :count 1}])
  ;#_=> true
  ;
  ;(contain-shiny-gold-bag? [{}])
  ;#_=> false

  (part1 "resources/day7/aoc-input1.txt")
  #_=> 177

  )

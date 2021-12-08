(ns clojure-practice.advent-of-code-2020.day8.handheld-halting
  (:require [clojure.string :as str]))

(comment
  "
  --- Day 8: Handheld Halting ---
  Your flight to the major airline hub reaches cruising altitude without incident. While you consider checking the in-flight menu for one of those drinks that come
  with a little umbrella, you are interrupted by the kid sitting next to you.

  Their handheld game console won't turn on! They ask if you can take a look.

  You narrow the problem down to a strange infinite loop in the boot code (your puzzle input) of the device. You should be able to fix it, but first you need to be
  able to run the code in isolation.

  The boot code is represented as a text file with one instruction per line of text. Each instruction consists of an operation (acc, jmp, or nop) and an argument
  (a signed number like +4 or -20) .

  acc increases or decreases a single global value called the accumulator by the value given in the argument. For example, acc +7 would increase the accumulator by 7.
  The accumulator starts at 0. After an acc instruction, the instruction immediately below it is executed next. jmp jumps to a new instruction relative to itself.
  The next instruction to execute is found using the argument as an offset from the jmp instruction ; for example, jmp +2 would skip the next instruction, jmp +1 would
  continue to the instruction immediately below it, and jmp -20 would cause the instruction 20 lines above to be executed next.
  nop stands for No OPeration - it does nothing. The instruction immediately below it is executed next.
  For example, consider the following program:

  nop +0
  acc +1
  jmp +4
  acc +3
  jmp -3
  acc -99
  acc +1
  jmp -4
  acc +6
  These instructions are visited in this order:

  nop +0 | 1
  acc +1 | 2, 8 (!)
  jmp +4 | 3
  acc +3 | 6
  jmp -3 | 7
  acc -99 |
  acc +1 | 4
  jmp -4 | 5
  acc +6 |
  First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1 (acc +1) and jmp +4 sets the next instruction to the other acc +1 near the bottom.
  After it increases the accumulator from 1 to 2, jmp -4 executes, setting the next instruction to the only acc +3. It sets the accumulator to 5, and jmp -3 causes
  the program to continue back at the first acc +1.
  This is an infinite loop: with this sequence of jumps, the program will run forever. The moment the program tries to run any instruction a second time, you know
  it will never terminate.
  Immediately before the program would run an instruction a second time, the value in the accumulator is 5.
  Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?"
  )

(defn ^:private str->instruction-map
  [str]
  (let [[instruction value] (str/split str #" ")]
    {:operation instruction
     :value     (Integer/parseInt value)}))

(defn input-str->instructions-data
  "convert given str to list of instructions data"
  [input-str]
  (->> input-str
       str/split-lines
       (map str->instruction-map)
       vec))

(defn accumulator-value-before-loop
  "accumulator value just before the instructions result in an infinite loop"
  [instructions-data]
  (loop [i   0
         {:keys [operation value]} (first instructions-data)
         ran #{}
         acc 0]
    #_(println {:index i
              :op    (str operation " " value)
              :acc   acc
              :ran   ran})
    (if (contains? ran i)
      acc
      (let [next-index (cond (= "jmp" operation) (+ i value)
                             :else (inc i))]                ;;conrner case
        (recur next-index
               (get instructions-data next-index)
               (conj ran i)
               (cond (= "acc" operation) (+ acc value)
                     :else acc))))))

(defn part1
  [input-file-path]
  (let [input-str  (slurp input-file-path)
        input-data (input-str->instructions-data input-str)]
    (accumulator-value-before-loop input-data)))


(comment
  (str->instruction-map "map +0")
  #_=> {:operation "map", :value 0}

  (str->instruction-map "acc -99")
  #_=> {:operation "acc", :value -99}

  (input-str->instructions-data (slurp "resources/day8/sample-input.txt"))
  #_=> [{:operation "nop", :value 0}
        {:operation "acc", :value 1}
        {:operation "jmp", :value 4}
        {:operation "acc", :value 3}
        {:operation "jmp", :value -3}
        {:operation "acc", :value -99}
        {:operation "acc", :value 1}
        {:operation "jmp", :value -4}
        {:operation "acc", :value 6}]

  (def ins-data (input-str->instructions-data (slurp "resources/day8/sample-input.txt")))
  ;;=> #'clojure-practice.advent-of-code-2020.day8.handheld-halting/ins-data

  (accumulator-value-before-loop (vec ins-data))
  ;{:index 0, :op nop 0, :acc 0, :ran #{}}
  ;{:index 1, :op acc 1, :acc 0, :ran #{0}}
  ;{:index 2, :op jmp 4, :acc 1, :ran #{0 1}}
  ;{:index 6, :op acc 1, :acc 1, :ran #{0 1 2}}
  ;{:index 7, :op jmp -4, :acc 2, :ran #{0 1 6 2}}
  ;{:index 3, :op acc 3, :acc 2, :ran #{0 7 1 6 2}}
  ;{:index 4, :op jmp -3, :acc 5, :ran #{0 7 1 6 3 2}}
  ;{:index 1, :op acc 1, :acc 5, :ran #{0 7 1 4 6 3 2}}
  #_=> 5

  (part1 "resources/day8/aoc-day8-part1.txt")
  #_=> 5

  )

(ns clojure-practice.random.pied-piper
  (:require [clojure.string :as str]))

(comment

  "The Pied Piper has been enlisted to play his magical tune and coax all the rats out of town.

  But some of the rats are deaf and are going the wrong way!

  How many deaf rats are there?

  Legend

  P = The Pied Piper
  O ~ = Rat going left
  ~O = Rat going right
  Example

  ex1 ~O ~O ~O ~O P has 0 deaf rats
  ex2 P O ~ O ~ ~O O ~ has 1 deaf rat
  ex3 ~O ~O ~O ~OP ~O ~OO~ has 2 deaf rats"

  )

(defn rats-direction-count
  [rats-str]
  (loop [rats       (partition 2 rats-str)
         left-rats  0
         right-rats 0]
    ; (println {:left-rats  left-rats :right-rats right-rats})
    (if rats
      (let [rat (first rats)]
        (recur (next rats)
               (if (= (last rat) \~)
                 (inc left-rats)
                 left-rats)
               (if (= (first rat) \~)
                 (inc right-rats)
                 right-rats)))
      {:left-rats  left-rats
       :right-rats right-rats})))

#_(defn where-is-piper?
    [input-str]
    (cond (= (first input-str) \P) :left
          (= (last input-str) \P) :right
          :else :random))

#_(defn deaf-rats-count*
    [rats-count piper-direction]
    (cond (= piper-direction :left) (-> rats-count
                                        :right-rats)
          (= piper-direction :right) (-> rats-count
                                         :left-rats)))
#_(defn deaf-rats-on-left-of-piper
  [rats-count]
  (-> rats-count
      :left-rats))

#_(defn deaf-rats-on-right-of-piper
  [rats-count]
  (-> rats-count
      :right-rats))

(defn parse
  [rats-piper-str]
  (let [input-str-with-no-spaces     (str/replace rats-piper-str " " "")
        [left-rats right-rats] (str/split input-str-with-no-spaces #"P" -1)]
    [left-rats right-rats])
  )

#_(defn deaf-rats-count
  [input]
  (let [input-str-with-no-spaces     (str/replace input
                                                  " "
                                                  "")
        rats-left-and-right-of-piper (str/split input-str-with-no-spaces
                                                #"P"
                                                -1)
        ;_                               (println "rats-on-left-and-right-of-piper: " rats-on-left-and-right-of-piper)
        [rats-count-left-of-piper rats-count-right-of-piper] (mapv rats-count
                                                                   rats-left-and-right-of-piper)
        ;_                               (println (str rats-count-left-of-piper " P " rats-count-right-of-piper))
        ]
    (+ (deaf-rats-on-left-of-piper rats-count-left-of-piper)
       (deaf-rats-on-right-of-piper rats-count-right-of-piper))))

(defn deaf-rats-count
  [[left-rats right-rats]]
  (+ (-> left-rats
         rats-direction-count
         :left-rats)
     (-> right-rats
         rats-direction-count
         :right-rats)))


;;REPL execution block
(comment
  (def rats-piper-str "~O ~O ~O ~O P")
  (parse rats-piper-str)
  #_=> ["~O~O~O~O" ""]

  (rats-direction-count "~O~O~O~O")
  #_=> {:left-rats 0, :right-rats 4}

  (rats-direction-count "~O~O~O~O~O~OO~")
  #_=> {:left-rats 1, :right-rats 6}

  (rats-direction-count "O~O~~OO~")
  #_=> {:left-rats 3, :right-rats 1}

  ;(deaf-rats-on-right-of-piper {:left-rats 3, :right-rats 1})
  ;#_=> 1
  ;
  ;(deaf-rats-on-left-of-piper {:left-rats 0, :right-rats 0})
  ;#_=> 0

  (deaf-rats-count "PO~O~~OO~")
  #_=> 1

  (def input-v ["PO~O~~OO~"
                "~O ~O ~O ~O P"
                "~O ~O ~O ~OP ~O ~OO~"
                "P"])

  (do (def parsed-input (mapv parse input-v))
      parsed-input)
  #_=> [["" "O~O~~OO~"] ["~O~O~O~O" ""] ["~O~O~O~O" "~O~OO~"] ["" ""]]

  (map deaf-rats-count parsed-input)
  #_=> (1 0 2 0)

  )

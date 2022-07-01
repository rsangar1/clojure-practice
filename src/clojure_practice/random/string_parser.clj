(ns clojure-practice.random.string-parser
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.string :as str]))

(comment
  "Write a function to parse any string of the following format into a map:"
  "status:321;someText:a text thing;startIndex:1234")

#_(s/def ::valid-string string?)

(defn str->kv-pair
  [any-str]
  (let [[key value] (str/split any-str #":" 2)]
    (when (and (string? key)
               (string? value))
      {(keyword key) value})))

(do (s/fdef str->kv-pair
            :args (s/cat :any-str string?)
            :ret map?)
    (st/instrument `str->kv-pair))

(defn str->map
  [input-str]
  (let [str-s (str/split input-str #";")]
    (->> str-s
         (map str->kv-pair)
         (reduce merge {}))))

(do (s/fdef str->map
            :args (s/cat :input-str string?)
            :ret map?)
    (st/instrument `str->map))



(comment
  (str->kv-pair "ab:cd")
  #_=> {:ab "cd"}

  (str->kv-pair "ab:cd:ef:gh")
  #_=> {:ab "cd:ef:gh"}

  (str/split "ab::cd" #":")
  #_=> ["ab" "" "cd"]

  (str/split "ab::cd" #":" 2)
  #_=> ["ab" ":cd"]

  (str->kv-pair "abcd")
  #_=> nil

  (str->kv-pair 23)
  ;Execution error - invalid arguments to clojure-practice.leetcode.string-parser/str->kv-pair at (string_parser.clj:51).
  ;23 - failed: string? at: [:any-str]

  (str->kv-pair "")
  #_=> nil

  (str->kv-pair nil)
  ;Execution error - invalid arguments to clojure-practice.leetcode.string-parser/str->kv-pair at (string_parser.clj:62).
  ;nil - failed: string? at: [:any-str]

  (str->map "status:321;someText:a text thing;startIndex:1234")
  #_=> {:status "321", :someText "a text thing", :startIndex "1234"}

  (str->map "")
  #_=> {}

  (str->map nil)
  ;Execution error - invalid arguments to clojure-practice.leetcode.string-parser/str->map at (string_parser.clj:65).
  ;nil - failed: string? at: [:input-str]

  (str->map "status:321")
  #_=> {:status "321"}

  (str->map "status")
  #_=> {}

  (str->map "status:321;asbace")
  #_=> {:status "321"}

  )


(ns clojure-practice.random.reverse-large-words
  (:require [clojure.string :as str]))

(comment
  "Write a function that takes in a string of one or more words and returns the same string,
  but with all words with five letters or more reversed.
  Strings passed in will consist of only letters and spaces.
  Be sure to keep the order of the words the same and only reverse the letters."
  )

(defn reverse-large-word
  [large-word-len word]
  (if (>= (count word) large-word-len)
    (apply str (reverse word))
    word))

(defn reverse-large-words
  [large-word-len input-str]
  (->> (str/split input-str #" ")
       (map #(reverse-large-word large-word-len %))
       (str/join " ")))


(comment

  (reverse-large-word 5 "abc")
  #_=> "abc"

  (reverse-large-word 5 "hello")
  #_=> "olleh"

  (reverse-large-words 5 "reverse only large words")
  #_=> "esrever only egral sdrow"

  (str/reverse "abc")

  )

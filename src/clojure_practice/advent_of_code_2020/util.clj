(ns clojure-practice.advent-of-code-2020.util)

(defn read-input-file
  [path]
  (slurp path))

(defn bin-str->decimal
  "converts given binary string into a decimal number"
  [binary-str]
  (read-string (str "2r" binary-str)))

(ns clojure-practice.util)

(defn read-file
  [path]
  (slurp path))

(defn insert-at-index
  "insert given value into a vector at specified index"
  [any-vec index value]
  (let [[first-part second-part] (split-at index any-vec)
        new-vec (vec (concat first-part
                             (list value)
                             (rest second-part)))]
    new-vec))

(defn bin-str->decimal
  "converts given binary string into a decimal number"
  [binary-str]
  (read-string (str "2r" binary-str)))

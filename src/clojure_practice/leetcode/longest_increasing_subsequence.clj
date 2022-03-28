(ns clojure-practice.leetcode.longest-increasing-subsequence)

;;https://leetcode.com/problems/longest-increasing-subsequence/

(defn longest-increasing-subsequence
  [v]
  (loop [i                 0
         incr-subseq-len-v []]
    (if (>= i (count v))
      incr-subseq-len-v
      (let [incr-subseq-len (loop [j               0
                                   incr-subseq-len 1]
                              (if (>= j i)
                                incr-subseq-len
                                (let [current-max (inc (nth incr-subseq-len-v j))
                                      subseq-len  (if (and (> (nth v i) (nth v j))
                                                           (< incr-subseq-len current-max))
                                                    current-max
                                                    incr-subseq-len)]

                                  (recur (inc j)
                                         subseq-len))))]
        (recur (inc i)
               (conj incr-subseq-len-v incr-subseq-len))))))

(apply max (longest-increasing-subsequence [2 100 101 3 4 5 102 103 10 11 20 21 6]))

[1 2 2 3 4]

;(memoize )

(comment

  (longest-increasing-subsequence [0, 1, 0, 3, 2, 3])
  #_=> 4

  (longest-increasing-subsequence [10, 9, 2, 5, 3, 7, 101, 18])
  #_=> 4

  (longest-increasing-subsequence [])
  #_=> 0

  (longest-increasing-subsequence [2])
  #_=> 1

  (longest-increasing-subsequence [2 3 1 7])
  #_=> 3

  [2 3 1 7]
  [1 2 1 3]




  )
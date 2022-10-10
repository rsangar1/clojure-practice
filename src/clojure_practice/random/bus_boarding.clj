(ns clojure-practice.random.bus-boarding
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check.generators :as test.gen]))

(comment
  " There is a bus moving in the city, and it takes and drops some people in each bus stop.
  You are provided with a list (or vector) of integer pairs. Elements of each pair represent
  the number of people getting onto bus (The first item) and number of people getting off of
  the bus (The second item) in a bus stop.
  Your task is to return number of people who are still on the bus after the last bus station
  (after the last array). Even though it is the last bus stop, the bus is not empty and some
  people are still in the bus.
  The second value in the first integer pair is 0, since the bus is empty in the first bus stop.")

(defn ->boarding-info-map
  [[onboard-count offboard-count]]
  {:onboard-count  onboard-count
   :offboard-count offboard-count
   :change-count   (- onboard-count offboard-count)})

(defn boarding-input->boarding-info
  [boarding-input]
  (->> boarding-input
       (map ->boarding-info-map)))

(defn onboard-bus-count
  [boarding-input]
  (->> boarding-input
       boarding-input->boarding-info
       (map :change-count)
       (apply +)))

#_(defn onboard-bus-count
    [[onboard-count offboard-count]]
    (->> boarding-data
         (map (fn [[onboard-count offboard-count]]
                (- onboard-count offboard-count)))
         (apply +)))

(comment

  (->boarding-info-map [3 0])
  #_=> {:onboard-count 3, :offboard-count 0, :change-count 3}

  (boarding-input->boarding-info [[3 0] [2 1] [1 3] [0 1]])
  #_=> ({:onboard-count 3, :offboard-count 0, :change-count 3}
        {:onboard-count 2, :offboard-count 1, :change-count 1}
        {:onboard-count 1, :offboard-count 3, :change-count -2}
        {:onboard-count 0, :offboard-count 1, :change-count -1})

  (map boarding-input->boarding-info [[]
                                      [[2 1]]
                                      [[3 0] [2 1] [1 3] [0 1]]])
  #_=> (()
        ({:onboard-count 2, :offboard-count 1, :change-count 1})
        ({:onboard-count 3, :offboard-count 0, :change-count 3}
         {:onboard-count 2, :offboard-count 1, :change-count 1}
         {:onboard-count 1, :offboard-count 3, :change-count -2}
         {:onboard-count 0, :offboard-count 1, :change-count -1}))

  (onboard-bus-count [[3 0] [2 1] [1 3] [0 1]])
  #_=> 1

  (map onboard-bus-count [[]
                          [[2 1]]
                          [[3 0] [2 1] [1 3] [0 1]]])
  #_=> (0 1 1)




  (s/def ::pos-int-or-zero? (s/or :pi pos-int? :z zero?))
  (s/def ::boarding-data (s/tuple ::pos-int-or-zero? ::pos-int-or-zero?))

  (s/valid? ::pos-int-or-zero? -1)

  (s/valid? ::boarding-data [2 1])

  (gen/generate (s/gen (s/coll-of ::boarding-data)))

  )

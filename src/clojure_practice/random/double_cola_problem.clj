(ns clojure-practice.random.double-cola-problem)

(comment
  "Sheldon, Leonard, Penny, Rajesh and Howard are in the queue for a  Double Cola  drink vending machine ; there are no other people in the queue. The first one in the queue (Sheldon) buys a can, drinks it and doubles! The resulting two Sheldons go to the end of the queue. Then the next in the queue (Leonard) buys a can, drinks it and gets to the end of the queue as two Leonards, and so on.

  Write a program that will return the name of the person who will drink the n-th cola.

  For example, Penny drinks the third can of cola and the queue will look like this: Rajesh, Howard, Sheldon, Sheldon, Leonard, Leonard, Penny, Penny

  The input data will consist of an array containing at least one name and a single integer n. Return the single line - the name of the person who drinks the n-th can of cola. The cans are numbered starting from 1."

  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 1) => "Sheldon"
  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 52) => "Penny"
  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 7230702951) => "Leonard")

(defn who-is-next
  [people n]
  (let [people-count (count people)]
    (loop [remainder (dec n)]
      (if (<= remainder people-count)
        (nth people remainder)
        (recur (/ (- remainder people-count) 2))))))



(defn double-list
  [people]
  )


(comment

  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 1)
  #_=> "Sheldon"

  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 52)
  #_=> "Penny"

  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 7230702951)
  #_=> "Leonard"


  )

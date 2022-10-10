(ns clojure-practice.random.walk_tracking_app)

(comment
  "You live in a city where all roads are laid out in a perfect grid. You have the unfortunate habit of arriving too
  early or too late to your appointments, so you decide to create a Walk Tracking App. You want create the application
  that tracks where you have walked and will then give you cardinal directions back to your starting point.
  (eg. ['n','s','w','e'] )
  Write a function that will take the currently walked path (e.g., ['n' 'e' 'e' ...]) and return the path that will
  lead you back to your origin.")

(def opposite-directions
  {\e \w
   \w \e
   \n \s
   \s \n})

(defn return-path
  [path]
  (->> path
       rseq
       (map opposite-directions)))

;;more readable?
#_(->> path
       rseq
       (map (fn [direction]
              (get opposite-directions direction))))


(defn return-path1
  [path]
  (reduce (fn [r-path direction]
            (conj r-path (opposite-directions direction)))
          '()
          path))

(comment
  (return-path [\n \e \w \n \e \s])
  #_=> (\n \w \s \e \w \s)

  (def paths [[\n \e \w \n \e \s]
              [\e \w \n \s]
              [\e \w \e \w \e \w]
              []])

  (map return-path paths)
  #_=> ((\n \w \s \e \w \s)
        (\n \s \e \w)
        (\e \w \e \w \e \w)
        ())


  (return-path1 '(\n \e \w \n \e \s))
  #_=> (\n \w \s \e \w \s)

  (map return-path1 paths)
  #_=> ((\n \w \s \e \w \s)
        (\n \s \e \w)
        (\e \w \e \w \e \w)
        ())
  )

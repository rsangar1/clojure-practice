(ns clojure-practice.random.online-coffee-distributor)

(comment
  "You are an enterprising coffee shop owner in Brew York City with 5 coffee shops beginning your ventures into a new
   fangled business called 'online shopping'. You will soon begin receiving coffee bean orders from online users,
   which you will need to send to your existing brick and mortar stores. You want to build a function (s) that will
   _deterministically_ pick a coffee shop to which a user's order should be sent. (If the same user makes subsequent
   online purchases, the same coffee shop should be sending their order)
  e.g.,
  (register-coffee-shops! [:brewed-awakening :espresso-yourself :the-daily-grind :latte-da :pour-decisions])

  (find-coffeeshop :joe-brewster)   #_=> :brewed-awakening
  (find-coffeeshop :decaf-daphne)   #_=> :pour-decisions
  (find-coffeeshop :iced-iris)      #_=> :the-daily-grind
  (find-coffeeshop :americano-andy) #_=> :pour-decisions
  (find-coffeeshop :joe-brewster)   #_=> :brewed-awakening
  Furthermore, when you open new coffeeshop (s), the new coffeeshop make takeover some orders from one (or more)
  existing coffeeshops, but it shouldn't cause other existing orders to change to which coffee shop they are routed.
  e.g.,
  (register-coffee-shop! :mugshot-cafe)

  (find-coffeeshop :joe-brewster)   #_=> :brewed-awakening
  (find-coffeeshop :decaf-daphne)   #_=> :pour-decisions
  (find-coffeeshop :iced-iris)      #_=> :the-daily-grind
  (find-coffeeshop :americano-andy) #_=> :mugshot-cafe
  MOST IMPORTANTLY, due to Brew York City privacy regulations, no personal information (name, etc.) or ANY type of
  information derived from user information may be saved/persisted!
  *** If you break this regulation, YOU WILL BE JAILED and your shops will be confiscated! ***
  Write the function (find-coffeeshop <user>) and any other functions/code necessary to accomplish your goal of being
  the world's finest online coffee distributor."
  )

(def coffee-shops (atom {}))

(defn sort-by-val
  [m]
  (into {} (sort-by val m)))

(defn register-coffee-shops!
  [shops]
  (let [coffee-shop-map (zipmap shops (map hash shops))]
    (reset! coffee-shops (sort-by-val coffee-shop-map))))

(defn register-coffee-shop!
  [shop]
  (let [coffee-shops-with-new-shop (assoc @coffee-shops shop (hash shop))]
    (reset! coffee-shops (sort-by-val coffee-shops-with-new-shop))))

(defn first-matching-coffee-shop
  [user]
  (some (fn [[coffee-shop hashed-coffee-shop]]
          (when (> hashed-coffee-shop (hash user))
            coffee-shop))
        @coffee-shops))

(defn find-coffee-shop
  [user]
  (let [target-copy-shop (first-matching-coffee-shop user)]
    (or target-copy-shop (key (first @coffee-shops)))))



;;REPL execution block
(comment

  (sort-by-val {:a 20 :b 10 :c 5 :d 25})
  #_=> {:c 5, :b 10, :a 20, :d 25}

  (register-coffee-shops! [:brewed-awakening :espresso-yourself :the-daily-grind :latte-da :pour-decisions])
  #_=> {:latte-da -2101923393,
        :brewed-awakening -1051147013,
        :pour-decisions -896746113,
        :the-daily-grind -45846930,
        :espresso-yourself 432620008}

  (first-matching-coffee-shop :joe-brewster)
  #_=> :brewed-awakening

  (first-matching-coffee-shop :decaf-daphne)
  #_=> nil

  (find-coffee-shop :decaf-daphne)
  #_=> :latte-da

  (map find-coffee-shop [:joe-brewster :decaf-daphne :iced-iris :americano-andy :joe-brewster] )
  #_=> (:brewed-awakening :latte-da :espresso-yourself :espresso-yourself :brewed-awakening)

  (register-coffee-shop! :mugshot-cafe)
  #_=> {:latte-da -2101923393,
        :brewed-awakening -1051147013,
        :pour-decisions -896746113,
        :the-daily-grind -45846930,
        :espresso-yourself 432620008,
        :mugshot-cafe 951306753}


  (map find-coffee-shop [:joe-brewster :decaf-daphne :iced-iris :americano-andy :joe-brewster] )
  #_=> (:brewed-awakening :latte-da :espresso-yourself :espresso-yourself :brewed-awakening)

  (register-coffee-shop! :indian-special)
  #_=> {:latte-da -2101923393,
        :brewed-awakening -1051147013,
        :pour-decisions -896746113,
        :the-daily-grind -45846930,
        :espresso-yourself 432620008,
        :mugshot-cafe 951306753,
        :indian-special 1613848381}

  (map find-coffee-shop [:joe-brewster :decaf-daphne :iced-iris :americano-andy :joe-brewster] )
  #_=> (:brewed-awakening :indian-special :espresso-yourself :espresso-yourself :brewed-awakening)

  (map find-coffee-shop [:joe-brewster :decaf-daphne :iced-iris :americano-andy :joe-brewster :random-user :coffee-lover] )
  #_=> (:brewed-awakening :indian-special :espresso-yourself :espresso-yourself :brewed-awakening :indian-special :the-daily-grind)

  )

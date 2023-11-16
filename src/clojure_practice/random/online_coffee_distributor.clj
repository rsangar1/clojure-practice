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

(def coffee-shops (atom []))

(register-coffee-shop!
  [shop]
  (swap! coffee-shops assoc shop))

(register-coffee-shops!
  [shops]
  (map register-coffee-shop shops))

(defn find-coffee-shop [user]
  (let [num-shops (count @coffee-shops)
        user-hash (hash user)
        shop-index (mod user-hash num-shops)]
    (nth @coffee-shops shop-index)))







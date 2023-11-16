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

(defn register-coffee-shops!
  [shops]
  (let [coffee-shop-hash-map (zipmap shops (map hash shops))
        sorted-coffee-shop-hash-map (into {} (sort-by val coffee-shop-hash-map))]
    (reset! coffee-shops coffee-shop-hash-map)))

(defn register-coffee-shop!
  [shop]
  (let [hashed-shop (hash shop)
        coffee-shops-with-new-shop (assoc @coffee-shops shop hashed-shop)
        new-sorted-coffee-shops (into {} (sort-by val coffee-shops-with-new-shop))]
    (reset! coffee-shops new-sorted-coffee-shops)))

(defn find-coffee-shop
  [user]
  (let [hashed-user (hash user)
        shop-keys (keys @coffee-shops)
        ]))

(defn first-matching-coffee-shop
  [hashed-user]
  (some (fn [[k v]]
          (when (> v hashed-user) k))
        @coffee-shops))

;(defn find-coffeeshop [user]
;  (let [hashed-user (hash-fn user)
;        shops (seq @coffee-shops)
;        ring (apply concat (for [[shop _] shops]
;                             (repeat virtual-nodes-per-shop shop)))
;        ring-sorted (sort ring)
;        target-shop (first (drop-while #(<= hashed-user %) ring-sorted))]
;    (or target-shop (first ring))))

;(defn register-coffee-shop! [new-shop]
;  (swap! coffee-shops assoc new-shop nil))

;(defn update-coffee-shop-rings! []
;  (let [all-shops (keys @coffee-shops)
;        total-virtual-nodes (* virtual-nodes-per-shop (count all-shops))]
;    (doseq [shop all-shops]
;      (swap! coffee-shops update shop (fn [_] (range (* virtual-nodes-per-shop (hash-fn shop))
;                                                     total-virtual-nodes
;                                                     virtual-nodes-per-shop))))))

(comment

  (register-coffee-shops! [:brewed-awakening :espresso-yourself :the-daily-grind :latte-da :pour-decisions])
  #_=> {:latte-da -2101923393,
        :brewed-awakening -1051147013,
        :pour-decisions -896746113,
        :the-daily-grind -45846930,
        :espresso-yourself 432620008}

  (register-coffee-shop! :mugshot-cafe)
  #_=> {:latte-da -2101923393,
        :brewed-awakening -1051147013,
        :pour-decisions -896746113,
        :the-daily-grind -45846930,
        :espresso-yourself 432620008,
        :mugshot-cafe 951306753}

  (first-matching-coffee-shop (hash :joe-brewster))
  #_=> :brewed-awakening

  (first-matching-coffee-shop (hash :decaf-daphne))

  (first-matching-coffee-shop (hash :iced-iris))


  )







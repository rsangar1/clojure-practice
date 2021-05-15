(ns clojure-practice.advent-of-code-2020.day4-passport-processing
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(comment
  "
  https://adventofcode.com/2020/day/4

  --- Day 4: Passport Processing ---
  You arrive at the airport only to realize that you grabbed your North Pole Credentials instead of your passport. While
  these documents are extremely similar, North Pole Credentials aren't issued by a country and therefore aren't actually
  valid documentation for travel in most of the world.

  It seems like you're not the only one having problems, though ; a very long line has formed for the automatic passport
  scanners, and the delay could upset your travel itinerary.

  Due to some questionable network security, you realize you might be able to solve both of these problems at the same time.

  The automatic passport scanners are slow because they're having trouble detecting which passports have all required
  fields. The expected fields are as follows:

  byr (Birth Year)
  iyr (Issue Year)
  eyr (Expiration Year)
  hgt (Height)
  hcl (Hair Color)
  ecl (Eye Color)
  pid (Passport ID)
  cid (Country ID)

  Passport data is validated in batch files (your puzzle input) . Each passport is represented as a sequence of key:value
  pairs separated by spaces or newlines. Passports are separated by blank lines.

  Here is an example batch file containing four passports:

  ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  byr:1937 iyr:2017 cid:147 hgt:183cm

  iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929

  hcl:#ae17e1 iyr:2013
  eyr:2024
  ecl:brn pid:760753108 byr:1931
  hgt:179cm

  hcl:#cfa07d eyr:2025 pid:166559648
  iyr:2011 ecl:brn hgt:59in

  The first passport is valid - all eight fields are present. The second passport is invalid - it is missing hgt
  (the Height field) .

  The third passport is interesting; the only missing field is cid, so it looks like data from North Pole Credentials,
  not a passport at all! Surely, nobody would mind if you made the system temporarily ignore missing cid fields. Treat
  this \"passport\" as valid.

  The fourth passport is missing two fields, cid and byr. Missing cid is fine, but missing any other field is not, so
  this passport is invalid.

  According to the above rules, your improved system would report 2 valid passports.

  Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file,
  how many passports are valid?")

(defn read-input-file
  [file-path]
  (slurp file-path))

(defn club-multiline-str
  [data]
  (loop [v      data
         p      ""
         result []]
    (if v
      (let [first (first v)]
        (recur (next v)
               (if (= first "")
                 ""
                 (str p " " first))
               (if (= first "")
                 (conj result p)
                 result)))
      (conj result p))))

(defn input-file->data
  [file-path]
  (let [input (read-input-file file-path)
        ;_     (print input)
        lines (str/split-lines input)]
    (club-multiline-str lines)))

(defn field->kv-pair
  [f]
  (let [[k v] (str/split f #":")]
    {(keyword k) v}))

(defn passport-str->passport-map
  [passport-str]
  (let [fields     (str/split (str/trim passport-str) #" ")
        fields-map (apply merge (map #(field->kv-pair %) fields))]
    fields-map))

(defn data->passport-details
  [input-data]
  (map #(passport-str->passport-map %) input-data))

(defn valid-passport?
  [p mandatory-fields]
  (let [p-fields       (set (keys p))
        missing-fields (set/difference mandatory-fields p-fields)
        valid?         (= (count missing-fields) 0)
        _              (println {:passport         p
                                 :mandatory-fields mandatory-fields
                                 :missing-fields   missing-fields
                                 :valid?           valid?})]
    valid?))

(defn validate-passports
  [passports mandatory-fields]
  (map #(valid-passport? % mandatory-fields) passports))

(defn count-valid-passports
  [input-file mandatory-fields]
  (let [input-data           (input-file->data input-file)
        passport-details-map (map #(passport-str->passport-map %) input-data)
        _                    (print passport-details-map)
        valid-passports-count (count (filter true? (validate-passports passport-details-map mandatory-fields)))]
    valid-passports-count))

(comment
  (read-input-file "resources/passport_processing_input1.txt")
  #_=>
  ;"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\r
  ; byr:1937 iyr:2017 cid:147 hgt:183cm\r
  ; \r
  ; iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\r
  ; hcl:#cfa07d byr:1929\r
  ; \r
  ; hcl:#ae17e1 iyr:2013\r
  ; eyr:2024\r
  ; ecl:brn pid:760753108 byr:1931\r
  ; hgt:179cm\r
  ; \r
  ; hcl:#cfa07d eyr:2025 pid:166559648\r
  ; iyr:2011 ecl:brn hgt:59in"

  (loop [xs     ["a" "b" "c" "" "d"]
         result ""]
    (if xs
      (let [x (first xs)]
        (recur (next xs)
               (if (= x "")
                 (str result " - ")
                 (str result " " x))
               ))
      result))
  #_=> " a b c -  d"

  (club-multiline-str ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                       "byr:1937 iyr:2017 cid:147 hgt:183cm"
                       ""
                       "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                       "hcl:#cfa07d byr:1929"
                       ""
                       "hcl:#ae17e1 iyr:2013"
                       "eyr:2024"
                       "ecl:brn pid:760753108 byr:1931"
                       "hgt:179cm"
                       ""
                       "hcl:#cfa07d eyr:2025 pid:166559648"
                       "iyr:2011 ecl:brn hgt:59in"])
  #_=> [" ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
        " iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
        " hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
        " hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"]

  (club-multiline-str ["a" "" "b" "c" "" "d" "e" "f"])
  #_=> [" a" " b c" " d e f"]

  (input-file->data "resources/passport_processing_input1.txt")
  #_=> [" ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
        " iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
        " hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
        " hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"]

  (field->kv-pair "ab:cd")
  #_=> {"ab" "cd"}

  (passport-str->passport-map " ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  #_=> {:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}

  (data->passport-details [" ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
                           " iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
                           " hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
                           " hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"])
  #_=> ({:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
        {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"}
        {:hcl "#ae17e1", :iyr "2013", :eyr "2024", :ecl "brn", :pid "760753108", :byr "1931", :hgt "179cm"}
        {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"})

  (valid-passport? {:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
                   #{:ecl :pid :eyr :hcl :byr :iyr :hgt})

  (def passport-details
    '({:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
     {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"}
     {:hcl "#ae17e1", :iyr "2013", :eyr "2024", :ecl "brn", :pid "760753108", :byr "1931", :hgt "179cm"}
     {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"}))
  #_=> #'clojure-practice.advent-of-code-2020.day4-passport-processing/passport-details
  (def mandatory-fields #{:ecl :pid :eyr :hcl :byr :iyr :hgt})
  #_=> #'clojure-practice.advent-of-code-2020.day4-passport-processing/mandatory-fields
  (validate-passports passport-details mandatory-fields)
  ;{:passport {:ecl gry, :pid 860033327, :eyr 2020, :hcl #fffffd, :byr 1937, :iyr 2017, :cid 147, :hgt 183cm}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{}, :valid? true}
  ;{:passport {:iyr 2013, :ecl amb, :cid 350, :eyr 2023, :pid 028048884, :hcl #cfa07d, :byr 1929}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{:hgt}, :valid? false}
  ;{:passport {:hcl #ae17e1, :iyr 2013, :eyr 2024, :ecl brn, :pid 760753108, :byr 1931, :hgt 179cm}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{}, :valid? true}
  ;{:passport {:hcl #cfa07d, :eyr 2025, :pid 166559648, :iyr 2011, :ecl brn, :hgt 59in}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{:byr}, :valid? false}
  #_=> (true false true false)

  (validate-passports [{:a "xy" :b 2} {:c "da"}] (set [:a]))
  ;{:passport {:a xy, :b 2}, :mandatory-fields #{:a}, :missing-fields #{}, :valid? true}
  ;{:passport {:c da}, :mandatory-fields #{:a}, :missing-fields #{:a}, :valid? false}
  #_=> (true false)

  (count-valid-passports "resources/passport_processing_input1.txt" mandatory-fields)
  ;({:ecl gry, :pid 860033327, :eyr 2020, :hcl #fffffd, :byr 1937, :iyr 2017, :cid 147, :hgt 183cm} {:iyr 2013, :ecl amb, :cid 350, :eyr 2023, :pid 028048884, :hcl #cfa07d, :byr 1929} {:hcl #ae17e1, :iyr 2013, :eyr 2024, :ecl brn, :pid 760753108, :byr 1931, :hgt 179cm} {:hcl #cfa07d, :eyr 2025, :pid 166559648, :iyr 2011, :ecl brn, :hgt 59in}){:passport {:ecl gry, :pid 860033327, :eyr 2020, :hcl #fffffd, :byr 1937, :iyr 2017, :cid 147, :hgt 183cm}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{}, :valid? true}
  ;{:passport {:iyr 2013, :ecl amb, :cid 350, :eyr 2023, :pid 028048884, :hcl #cfa07d, :byr 1929}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{:hgt}, :valid? false}
  ;{:passport {:hcl #ae17e1, :iyr 2013, :eyr 2024, :ecl brn, :pid 760753108, :byr 1931, :hgt 179cm}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{}, :valid? true}
  ;{:passport {:hcl #cfa07d, :eyr 2025, :pid 166559648, :iyr 2011, :ecl brn, :hgt 59in}, :mandatory-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr}, :missing-fields #{:byr}, :valid? false}
  #_=> 2
  )
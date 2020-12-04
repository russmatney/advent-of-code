(ns four.core
  (:require [util :refer [input]]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn passports [f]
  (->> (input f)
       (partition-by #{""})
       (remove (comp #{""} first))
       (map #(string/join " " %))
       (map #(string/split % #" "))
       )
  )

(comment
  (passports "example.txt")
  (passports "input.txt")
  )

(defn fields [f]
  (->> (passports f)
       (map (fn [pp]
              (->> pp
                   (map #(string/split % #":"))
                   (map first)
                   (into #{}))))
       ))

(comment
  (fields "example.txt")

  (fields "input.txt")
  )

(defn fields-and-vals [f]
  (->> (passports f)
       (map (fn [pp]
              (->> pp
                   (map #(string/split % #":")))))))


(defn all-required-fields [f]
  (let [required #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}]
    (->> (fields f)
         (filter (fn [fs] (set/subset? required fs)))
         count)))

(comment
  (all-required-fields "example.txt")
  (all-required-fields "input.txt")
  ;; 266
  ;; 419
  ;; 210

  (set/subset? #{"a" "b" "c"} #{"a" "b" "c" "d"})

  )

(def validators
  {"byr" (fn [val] (-> val read-string (#(<= 1920 % 2002))))
   "iyr" (fn [val] (-> val read-string (#(<= 2010 % 2020))))
   "eyr" (fn [val] (-> val read-string (#(<= 2020 % 2030))))
   "hgt" (fn [val]
           (when-let [res (->> val (re-seq #"^(\d+)([cm|in]+)$") first)]
             (let [[_ d unit] res
                   d          (read-string d)]
               (cond
                 (= unit "cm") (<= 150 d 193)
                 (= unit "in") (<= 59 d 76)
                 :else         false))))
   "hcl" (fn [val] (re-seq #"^#[0-9a-f]{6}$" val))
   "ecl" (fn [val] (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} val))
   "pid" (fn [val] (re-seq #"^[0-9]{9}$" val))

   "cid" (constantly false)
   })

(comment
  (re-seq #"[0-9]{9}" "1234567890")
  (re-seq #"^[0-9]{9}$" "1234567890")

  ((constantly true) "blah")
  (-> "1920" read-string (#(<= 1920 % 2002)))
  (re-seq #"#[0-9a-f]{6}" "#123456")
  (re-seq #"#[0-9a-f]{6}" "123456")
  (re-seq #"(\d+)([cm|in]+)" "123cm"))

(defn all-valid-fields [f]
  (let [required #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}]
    (->> (fields-and-vals f)
         (filter (fn [f-vs]
                   (->> f-vs
                        (map first)
                        (into #{})
                        (#(set/subset? required %)))))
         (filter (fn [f-vs]
                   (let [valid-fields
                         (->> f-vs
                              (filter
                                (fn [[field val]]
                                  ;; (println field val)
                                  ((get validators field) val))))]
                     (println valid-fields)
                     (println (count valid-fields))
                     (= (count valid-fields) (count required)))))
         count)))

(comment
  (println "break")
  (all-valid-fields "valid.txt")
  (all-valid-fields "invalid.txt")
  (all-valid-fields "example.txt")
  (all-valid-fields "input.txt")
  ;; 132
  ;; 62
  )

(ns _2021.eight.core
  (:require [util :refer [input]]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-line [l]
  (let [[before after] (string/split l #" \| ")
        p              #(->> (string/split % #" "))]
    {:patterns (p before)
     :output   (p after)}))

(defn parse [f] (->> (input f) (map parse-line)))

(defn all-output [lines]
  (->> lines (mapcat :output)))

(defn unique-digit? [d]
  (#{#_1 2 #_4 4 #_7 3 #_8 7} (count d)))

(comment
  (->>
    (parse "input.txt")
    all-output
    (filter unique-digit?)
    count))

;; part 2

(def number->seg-count
  {0 6
   1 2
   2 5
   3 5
   4 4
   5 5
   6 6
   7 3
   8 7
   9 6})

(def seg-count->numbers (->> number->seg-count
                             (group-by val)
                             (map (fn [[k vs]] [k (map first vs)]))
                             (into {})))

(defn decode [{:keys [one four seven six _eight]} signal]
  (let [len (count signal)]
    (cond
      ;; 0 6 9
      (#{6} len)
      (cond
        (set/subset? four (set signal))  9
        (set/subset? seven (set signal)) 0
        :else                            6)

      ;; 2 3 5
      (#{5} len)
      (cond
        (set/subset? one (set signal)) 3

        (not (seq six))
        ;; "no six yet, cannot decode!"
        nil

        (= 1 (count (set/difference six (set signal)))) 5
        :else                                           2)

      :else (first (seg-count->numbers len)))))

(comment
  (set/subset? (set "abcd") (set "abcdef"))
  (decode "abcdef" {:one   (set "ab")
                    :four  (set "abcd")
                    :seven (set "abc")}))

(defn parse-knowns [line]
  (let [signals   (->> line (vals) (apply concat))
        for-len-n (fn [n] (->> signals (filter #(#{n} (count %))) first set))]
    {:one   (for-len-n 2)
     :four  (for-len-n 4)
     :seven (for-len-n 3)
     :eight (for-len-n 7)}))

(comment
  (parse-knowns
    {:patterns #{"cgeb" "edb" "fabcd" "agebfd" "fdcge" "be" "cbdgef" "fecdb" "fgaecd" "cfbegad"}, :output #{"fdgacbe" "gcbe" "cefdb" "cefbgd"}}))

(defn decode-line [{:keys [output patterns] :as line}]
  (let [knowns  (parse-knowns line)
        signals (concat output patterns)

        ;; make sure we've decoded :six
        knowns (loop [remaining signals
                      knowns    knowns]
                 (if (or (not (seq remaining))
                         (#{:one :four :seven :six} knowns))
                   knowns
                   (let [next    (first remaining)
                         rst     (rest remaining)
                         decoded (decode knowns next)
                         knowns  (case decoded
                                   6   (assoc knowns :six (set next))
                                   nil knowns
                                   knowns)]
                     (recur rst knowns))))]

    (->> output (map (partial decode knowns)))))

(comment
  (decode-line
    {:patterns #{"cgeb" "edb" "fabcd" "agebfd" "fdcge" "be" "cbdgef" "fecdb" "fgaecd" "cfbegad"}
     :output   #{"fdgacbe" "gcbe" "cefdb" "cefbgd"}})

  (->>
    (parse "example.txt")
    (map decode-line)
    (map #(->> % (apply str) (Integer.)))
    (apply +))

  (->>
    (parse "input.txt")
    (map decode-line)
    (map #(->> % (apply str) (Integer.)))
    (apply +)))

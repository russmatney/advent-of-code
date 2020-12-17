(ns sixteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(defn parse-rules [rules]
  (->> rules
       (map (fn [line]
              (let [res                    (re-seq #"([\w| ]+): (\d+)-(\d+) or (\d+)-(\d+)"
                                                   line)
                    [_ label & range-vals] (first res)
                    [a b x y]              (->> range-vals (map read-string))
                    ]
                {:label  label
                 :ranges [[a b] [x y]]})))))

(defn parse [f]
  (->> (input f)
       (partition-by #{""})
       (remove (comp #{""} first))
       ((fn [[rules your nearby]]
          {:rules          (parse-rules rules)
           :your-ticket    (-> your second
                               (string/split #",")
                               (->> (map read-string)))
           :nearby-tickets (->> nearby
                                rest
                                (map (fn [line]
                                       (->> (string/split line #",")
                                            (map read-string)))))}))))

(comment
  (parse "example.txt")
  (parse "input.txt"))

(defn val-in-any-range? [ranges v]
  (when v
    (->> ranges
         (filter
           (fn [[a b]]
             (<= a v b)))
         first)))

(defn error-rate [f]
  (let [parsed  (parse f)
        rules   (:rules parsed)
        ranges  (->> rules (mapcat :ranges))
        tickets (:nearby-tickets parsed)
        values  (->> tickets (mapcat (fn [x] x)))]
    (->> values
         (remove (partial val-in-any-range? ranges))
         (apply +))))

(comment
  (error-rate "example.txt")
  (error-rate "input.txt")
  )

;; part 2


(defn valid-tickets [f]
  (let [parsed  (parse f)
        ranges  (->> parsed :rules (mapcat :ranges))
        tickets (:nearby-tickets parsed)
        ]
    (->> tickets
         (remove
           (fn [ticket]
             (->> ticket
                  (remove (partial val-in-any-range? ranges))
                  first))))))

(comment
  (:rules (parse "example-two.txt"))
  (valid-tickets "example.txt")
  (valid-tickets "example-two.txt"))

(defn possible-indexes [{:keys [ranges]} tickets]
  (->> (range 0 (count tickets))
       (filter
         (fn [i]
           (->> tickets
                (map
                  (fn [ticket]
                    (if (val-in-any-range? ranges (nth ticket i nil))
                      true
                      false)))
                (every? true?))))
       (into #{})))

(defn rules-with-possible-indexes [f]
  (let [rules   (:rules (parse f))
        tickets (valid-tickets f)]
    (->> rules
         (map (fn [rule]
                (assoc rule :ixs (possible-indexes rule tickets)))))))

(defn rules-with-index [f]
  (->>
    (rules-with-possible-indexes f)
    (sort-by (comp count :ixs))
    ((fn [rules]
       (loop [with-index []
              rules      rules]
         (if-let [rule (some-> rules first)]
           ;; assumes there is only one of these, which happens to be the case
           (let [ix (first (:ixs rule))]
             (when (> (count (:ixs rule)) 1)
               (println "DERP"))
             (recur
               (conj with-index (assoc rule :ix ix))
               (->> rules rest (map (fn [rule]
                                      (update rule :ixs disj ix))))))
           with-index))))))

(comment
  (rules-with-possible-indexes "example-two.txt")
  (rules-with-index "example-two.txt")
  )

(defn ticket-with-labels [f]
  (let [rules       (rules-with-index f)
        rule-map    (->> rules (map (fn [r] [(:ix r) r])) (into {}))
        your-ticket (:your-ticket (parse f))]
    (->> your-ticket
         (map-indexed
           (fn [i v]
             [(:label (rule-map i)) v]))
         (into {}))))

(comment
  (ticket-with-labels "example-two.txt")
  (ticket-with-labels "input.txt")

  (->>
    (ticket-with-labels "input.txt")
    (filter (fn [[k _v]]
              (string/starts-with? k "departure")))
    (map second)
    (apply *))
  )

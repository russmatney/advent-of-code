(ns _2021.fourteen.core
  (:require [util :refer [input]]))

(defn parse-rule [rule]
  (->> rule
       (re-seq #"([A-Z])([A-Z]) -> ([A-Z])")
       first rest
       ((fn [[start end insert]]
          [(str start end) insert]))))

(comment
  (parse-rule "CH -> B"))

(defn parse [f]
  (->> f
       input
       util/partition-by-newlines
       ((fn [[template rules]]
          {:template (first template)
           :rules    (->> rules (map parse-rule) (into {}))}))))

(def ex-data (parse "example.txt"))
(def input-data (parse "input.txt"))

(defn apply-rules [rules polymer]
  (loop [[next & rst] polymer
         new-polymer  ""
         last         nil]
    (cond
      (not next)
      new-polymer

      (not last)
      (recur rst next next)

      :else
      (recur rst
             (str new-polymer (rules (str last next)) next)
             next))))

(defn do-steps [n {:keys [template rules]}]
  (->>
    (iterate (partial apply-rules rules) template)
    (take (inc n))
    last))

(defn counts [n data]
  (->>
    (do-steps n data)
    seq
    (group-by identity)
    (map (comp count second))))

(comment
  (do-steps 1 ex-data)
  (do-steps 2 ex-data)
  (do-steps 3 ex-data)

  (->>
    (counts 10 ex-data)
    sort
    ((fn [xs]
       (- (last xs) (first xs)))))

  ;; part 1
  (->>
    (counts 10 input-data)
    sort
    ((fn [xs]
       (- (last xs) (first xs)))))
  )

;; part 2

(defn apply-rules-2 [rules buckets]
  (->> buckets
       (map (fn [[match ct]]
              (when-let [new (rules match)]
                (let [[a b] new]
                  [[a ct]
                   [b ct]
                   [match (* -1 ct)]]))))
       (remove nil?)
       (apply concat)
       (reduce
         (fn [buck [k v]]
           (update buck k #(+ (or % 0) v)))
         buckets)))

(defn ->buckets [polymer]
  (->>
    (partition 2 1 polymer)
    (group-by identity)
    (map (fn [[k vs]] [(apply str k) (count vs)]))
    (into {})))

(defn ->rules [rules]
  (->> rules
       (map (fn [[[a b] c]]
              [(str a b) [(str a c) (str c b)]]))
       (into {})))

(comment
  (->buckets "NNCB")
  (->rules (:rules ex-data))
  (->buckets (:template input-data))
  (->rules (:rules input-data))

  (apply-rules-2 (->rules (:rules ex-data)) (->buckets (:template ex-data)))
  (apply-rules-2 (->rules (:rules input-data)) (->buckets (:template input-data)))
  )

(defn do-steps-2 [n {:keys [template rules]}]
  (->>
    (iterate (partial apply-rules-2 (->rules rules)) (->buckets template))
    (take (inc n))
    last))

(comment
  (do-steps-2 1 ex-data)
  (do-steps-2 2 ex-data)
  )

(defn counts-2 [n data]
  (let [lst (last (:template data))]
    (->>
      (do-steps-2 n data)
      (map (fn [[[a _b] v]] [a v]))
      (group-by first)
      (map (fn [[k vs]] [k (->> vs (map second) (apply +))]))
      (into {})
      ((fn [m] (update m lst inc))))))

(comment
  (do-steps-2 10 ex-data)
  (counts-2 1 ex-data)

  (->>
    (counts-2 40 ex-data)
    (sort-by second)
    ((fn [xs]
       (- (second (last xs)) (second (first xs)))))
    )

  (->>
    (counts-2 40 input-data)
    (sort-by second)
    ((fn [xs]
       (- (second (last xs)) (second (first xs)))))
    )
  )

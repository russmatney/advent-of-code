(ns nineteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(comment
  (input "example.txt")
  (input "input.txt")
  (println "hello!")
  )

(defn parse-rule [line]
  ;; TODO expand to handle non number rules
  (when (seq line)
    (some->> line
             (re-seq #"(\d+): ([\d| ]+)")
             first
             rest
             ((fn [[rule-number rule-refs]]
                (when rule-number
                  (println rule-number rule-refs)
                  [rule-number
                   (-> rule-refs
                       (string/split #" \|")
                       (->>
                         (map (fn [rule-nums]
                                (-> rule-nums
                                    string/trim
                                    (string/split #" ")
                                    (->>
                                      (map string/trim)
                                      (map read-string)))))))]))))))

(comment
  (parse-rule "0: 4 1 5")
  (parse-rule "0: 2 3 | 3 2")
  )

(defn rules [f]
  (->> f input
       (partition-by #{""})
       (remove (comp #{""} first))
       first
       (map parse-rule)
       (remove nil?)
       (into {})
       ))

(comment
  (rules "example.txt"))


(defn messages [f]
  (->> f input
       (partition-by #{""})
       (remove (comp #{""} first))
       second))

(defn valid? [rules message]
  true)

(comment
  (let [f              "example.txt"
        rules          (rules f)
        messages       (messages f)
        valid-messages (->> messages
                            (filter (partial valid? rules)))
        ]
    (concat
      ["Rules"]
      rules
      ;; ["Messages"]
      ;; messages
      ["Valid messages"]
      valid-messages))
  )

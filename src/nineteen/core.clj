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
             (re-seq #"(\d+): \"?([\w|\d| ]+)\"?")
             first
             rest
             ((fn [[rule-number rule-defs]]
                (when rule-number
                  (println rule-number rule-defs)
                  [rule-number
                   (-> rule-defs
                       (string/split #" \|")
                       (->>
                         (map (fn [rule-def]
                                (-> rule-def
                                    string/trim
                                    (string/split #" ")
                                    (->>
                                      (map string/trim)
                                      (map read-string)))))))]))))))

(comment
  (parse-rule "0: 4 1 5")
  (parse-rule "0: 2 3 | 3 2")
  (parse-rule "0: 2 3 | 3 2 | 1")
  (parse-rule "0: \"a\"")
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

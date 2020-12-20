(ns nineteen.core
  (:require [util :refer [input]]))

(comment
  (input "example.txt")
  (input "input.txt")
  (println "hello!")
  )

(defn rules [f]
  (->> f input
       (partition-by #{""})
       (remove (comp #{""} first))
       first))

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
      rules
      messages
      ["Valid messages"]
      valid-messages))
  )

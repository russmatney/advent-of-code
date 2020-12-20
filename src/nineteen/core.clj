(ns nineteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]
            [instaparse.core :as insta]))

(defn parser [f]
  (->> f
       input
       (partition-by #{""})
       (remove (comp #{""} first))
       first
       sort
       (string/join "\n")
       insta/parser
       ))

(comment
  (parser "example.txt")
  ((parser "input.txt") "aaabab")
  ((parser "example.txt") "aaabab"))

(defn messages [f]
  (->> f input
       (partition-by #{""})
       (remove (comp #{""} first))
       second))

(defn valid-count [f]
  (let [parse          (parser f)
        messages       (messages f)
        valid-messages (->> messages
                            (map parse)
                            (remove insta/failure?))]
    (count valid-messages)))

(comment
  (valid-count "example.txt")
  (valid-count "input.txt")
  (valid-count "input-two.txt")
  )

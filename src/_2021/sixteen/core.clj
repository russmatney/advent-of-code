(ns _2021.sixteen.core
  (:require [util :refer [input]]))

(def input-data (first (input "input.txt")))

(def hex->dec
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(comment
  (Integer/parseInt "001" 2)
  (Integer/parseInt "110" 2)
  (Integer/parseInt "11011" 2)

  (Integer/parseInt "11010001010" 2)
  (Integer/parseInt "1010" 2)
  (Integer/parseInt "0101001000100100" 2)
  )

(defn parse [inp]
  (->> inp
       (map hex->dec)
       (apply str)))

(comment
  input-data
  (parse "D2FE28")
  (parse input-data)
  )

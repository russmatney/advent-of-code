(ns _2024._03.core
  (:require [util :as util]))

(defn input [fname]
  (->>
    (util/parse-input (str "src/_2024/_03/" fname))
    (apply str)))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn parse-ops [s]
  (re-seq #"(mul)\((\d{1,3}),(\d{1,3})\)" s))

(comment
  (->> (input "input.txt") parse-ops)
  (parse-ops "xmul(2,4)%&do()blah"))

(defn calc-op [op]
  (let [[_match cmd arg1 arg2] op]
    (case cmd
      "mul" (* (read-string arg1) (read-string arg2)))))

(comment
  (->> (input "example.txt") parse-ops (map calc-op) (reduce + 0))
  (->> (input "input.txt") parse-ops
       (map calc-op) (reduce + 0))

  (->> (input "example.txt")
       (parse-ops)))

(defn calc-part-2 [s]
  (->> s
       (re-seq #"(mul)\((\d{1,3}),(\d{1,3})\)|(do)\(\)|(don't)\(\)")
       (reduce
         (fn [{:keys [on] :as agg} op]
           (let [[_match _mul _arg1 _arg2 do dont] op]
             (cond
               do    (assoc agg :on true)
               dont  (assoc agg :on false)
               on    (update agg :sum + (calc-op op))
               :else agg)))
         {:on true :sum 0})))

(comment
  (->>
    (input "input.txt")
    (calc-part-2)))

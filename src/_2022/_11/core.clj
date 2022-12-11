(ns _2022._11.core
  (:require
   [util :as util]
   [clojure.string :as string]))

(defn path [f]
  (str "src/_2022/_11/" f))

(defn monkeys [f]
  (->> (util/parse-input (path f) {:partition? true :trim? true})
       (map (fn [lines]
              (let [[m s o t tt tf] lines
                    m               (second (re-find #"Monkey (\d):" m))
                    items           (second (re-find #"Starting items: ([\d, ]+)" s))
                    items           (some->> (string/split items #", ") (map read-string) (into []))
                    op              (second (re-find #"Operation: new = old (.*)" o))
                    op-num          (or (some-> (re-find #"(\d+)" op) second read-string) :op/old)
                    op-op           (case (-> (string/split op #" ") first)
                                      "*" :op/mult "+" :op/add)
                    op              (cond
                                      (re-seq #"\* \d+" op)
                                      (-> (re-find #"\* (\d+)" op) second read-string
                                          ((fn [d] (fn [n] (* d n)))))

                                      (re-seq #"\+ \d+" op)
                                      (-> (re-find #"\+ (\d+)" op) second read-string
                                          ((fn [d] (fn [n] (+ d n)))))

                                      (re-seq #"\* old" op)
                                      (fn [n] (* n n)))
                    test-by      (-> (re-find #"Test: divisible by (\d+)" t) second read-string)
                    test-true-m  (second (re-find #"If true: throw to monkey (\d+)" tt))
                    test-false-m (second (re-find #"If false: throw to monkey (\d+)" tf))
                    next-m       (fn [n] (if (zero? (mod n test-by)) test-true-m test-false-m))]
                [m {:m            m
                    :items        items
                    :op           op
                    :op-num       op-num
                    :op-op        op-op
                    :test-by      test-by
                    :test-true-m  test-true-m
                    :test-false-m test-false-m
                    :next-m       next-m}])))
       (into {})))

(comment
  (re-find #"Monkey (\d):" "Monkey 7:")
  (re-find #"Starting items: ([\d, ]+)" "Starting items: 57, 59, 97")
  (->
    (monkeys "input.txt")
    (get "0")
    :op
    ((fn [op] (op 2))))

  (->
    (monkeys "input.txt")
    (get "0")
    :next-m
    ((fn [f] (f 19))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

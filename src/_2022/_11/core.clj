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
                    m               (-> (re-find #"Monkey (\d):" m) second read-string)
                    items           (second (re-find #"Starting items: ([\d, ]+)" s))
                    items           (some->> (string/split items #", ") (map read-string))
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
                    test-true-m  (-> (re-find #"If true: throw to monkey (\d+)" tt) second read-string)
                    test-false-m (-> (re-find #"If false: throw to monkey (\d+)" tf) second read-string)
                    next-m       (fn [n] (if (zero? (mod n test-by)) test-true-m test-false-m))]
                [m {:m             m
                    :items         items
                    :worry-op      op
                    :op-num        op-num
                    :op-op         op-op
                    :test-by       test-by
                    :test-true-m   test-true-m
                    :test-false-m  test-false-m
                    :next-m        next-m
                    :inspect-count 0}])))
       (into {})))

(comment
  (re-find #"Monkey (\d):" "Monkey 7:")
  (re-find #"Starting items: ([\d, ]+)" "Starting items: 57, 59, 97")
  (->
    (monkeys "input.txt")
    (get "0")
    :op
    ((fn [op] (op 2))))

  (->> (string/split "57 59 97" #" ")
       (map read-string)
       (apply list)
       peek
       )

  (->
    (monkeys "input.txt")
    (get "0")
    :next-m
    ((fn [f] (f 19))))

  (pop (list 7 8 9))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn worry-drop [n]
  (int (/ n 3)))

(defn inspect-item [monkeys m]
  (let [{:keys [items worry-op next-m]
         :as   _monkey} (get monkeys m)
        items           (->> items (apply list))
        new-items       (pop items)
        item            (peek items)
        new-worry-level (-> item worry-op worry-drop)
        next-monkey     (next-m new-worry-level)]
    (-> monkeys
        (assoc-in [m :items] new-items)
        (update-in [m :inspect-count] inc)
        (update-in [next-monkey :items] #(concat % [new-worry-level])))))

(comment
  (inspect-item (monkeys "example.txt") 0))

(defn full-turn [monkeys m]
  (loop [mks monkeys]
    (let [mk (get mks m)]
      (if (empty? (seq (:items mk)))
        mks
        (recur (inspect-item mks m))))))

(comment
  (full-turn (monkeys "example.txt") 0))

(defn full-round [monkeys]
  (->> (range (count monkeys))
       (reduce
         (fn [mks m]
           (full-turn mks m))
         monkeys)))

(comment
  (full-round (monkeys "example.txt")))

(defn sim-rounds [f n]
  (let [monkeys (monkeys f)]
    (->> (range n)
         (reduce (fn [mks _] (full-round mks)) monkeys))))

(defn monkey-biz [mks]
  (->> mks
       vals
       (sort-by :inspect-count >)
       (take 2)
       (map :inspect-count)
       (apply *)))

(comment
  (sim-rounds "example.txt" 20)
  (sim-rounds "input.txt" 20)

  (monkey-biz *1)
  )

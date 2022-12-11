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
                    worry-op        (cond
                                      (#{:op/old} op-num) (fn [n] (* n n))
                                      (#{:op/mult} op-op) (fn [n] (* op-num n))
                                      (#{:op/add} op-op)  (fn [n] (+ op-num n)))
                    test-by         (-> (re-find #"Test: divisible by (\d+)" t) second read-string)
                    test-true-m     (-> (re-find #"If true: throw to monkey (\d+)" tt) second read-string)
                    test-false-m    (-> (re-find #"If false: throw to monkey (\d+)" tf) second read-string)
                    next-m          (fn [n] (if (zero? (mod n test-by)) test-true-m test-false-m))]
                [m {:m             m
                    :items         items
                    :worry-op      worry-op
                    :op-num        op-num
                    :op-op         op-op
                    :test-by       test-by
                    :test-true-m   test-true-m
                    :test-false-m  test-false-m
                    :next-m        next-m
                    :inspect-count 0}])))
       (into {})
       ((fn [mks]
          (assoc mks :lcm (->> mks vals (map :test-by) (apply *)))))))

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

  (->> (string/split "57 59 97" #" ")
       (map read-string)
       (apply list)
       peek)
  (pop (list 7 8 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn worry-drop [n]
  (int (/ n 3)))

(defn inspection-result
  [item {:keys [worry-op next-m]} worry-drop? lcm]
  (let [new-worry-level (cond-> item
                          true                        worry-op
                          worry-drop?                 worry-drop
                          (and (not worry-drop?) lcm) (mod lcm))

        next-monkey (next-m new-worry-level)]
    {:new-worry-level new-worry-level
     :next-monkey     next-monkey}))

(comment
  (let [mks (monkeys "example.txt")]
    (inspection-result 79 (get mks 0) nil (:lcm mks))))

(defn inspect-item [{:keys [worry-drop? lcm] :as monkeys} m]
  (let [{:keys [items]
         :as   monkey} (get monkeys m)
        items          (->> items (apply list))
        new-items      (pop items)
        item           (peek items)
        {:keys [new-worry-level next-monkey]}
        (inspection-result item monkey worry-drop? lcm)]
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

(defn sim-rounds
  ([f n] (sim-rounds f n nil))
  ([f n include-worry-drop?]
   (let [monkeys (monkeys f)
         monkeys (if include-worry-drop?
                   (assoc monkeys :worry-drop? true)
                   monkeys)]
     (->> (range n)
          (reduce (fn [mks _] (full-round mks)) monkeys)))))

(defn log-monkey-biz [mks]
  (->> mks
       vals
       (filter map?)
       (sort-by :inspect-count >)
       (map (fn [m] (println "Monkey" (:m m) "inspected" (:inspect-count m) "items")))))

(defn monkey-biz [mks]
  (->> mks
       vals
       (filter map?)
       (sort-by :inspect-count >)
       (take 2)
       (map :inspect-count)
       (apply *)))

(comment
  ;; part 1
  (sim-rounds "example.txt" 20 true)
  (sim-rounds "input.txt" 20 true)
  (log-monkey-biz *1)
  (monkey-biz *1)

  ;; part 2
  (sim-rounds "example.txt" 20)
  (sim-rounds "input.txt" 20)
  (log-monkey-biz *1)

  (sim-rounds "example.txt" 10000)
  (sim-rounds "input.txt" 10000)

  (monkey-biz *1))

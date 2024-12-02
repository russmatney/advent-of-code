(ns _2024._02.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2024/_02/" fname)
                    {:ints? true :split? true}))

(comment
  (input "example.txt"))


(defn is-safe? [report]
  (->>
    report
    (reduce
      (fn [{:keys [last
                   prev-op
                   result
                   ]} next]
        (cond
          (= false result)          {:result false}
          (nil? last)               {:last next}
          (> (abs (- last next)) 3) {:result false}
          (= last next)             {:result false}

          (nil? prev-op)
          (cond
            (> next last) {:last next :prev-op >}
            (< next last) {:last next :prev-op <})

          :else
          {:result  (prev-op next last)
           :prev-op prev-op
           :last    next}))
      {:last nil :prev-op nil})
    ((fn [{:keys [result]}]
       (println report ":" result)
       result))))

(comment
  (is-safe? [0 1 2 5])
  (is-safe? [0 1 2 6])
  (is-safe? [0 3 5])
  (is-safe? [0 4 5])
  (is-safe? [5 3 2])
  (is-safe? [5 3 0])

  (->>
    (input "input.txt")
    (filter is-safe?)
    count))

(defn permutes [report]
  (->>
    report
    count
    range
    (map
      (fn [i]
        (->>
          [(take i report) (drop (inc i) report)]
          (mapcat identity)
          (into [])))))
  )

(comment
  (permutes [0 1 2])

  (->>
    (input "input.txt")
    (filter (fn [report]
              (if (is-safe? report)
                true
                (let [perms (permutes report)]
                  (->> perms
                       (filter is-safe?)
                       seq
                       first)))))
    count))

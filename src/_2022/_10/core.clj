(ns _2022._10.core
  (:require
   [clojure.core.match :refer [match]]
   [util :as util]))

(defn path [f]
  (str "src/_2022/_10/" f))

(defn input [f]
  (->>
    (util/parse-input (path f) {:split? true})
    (map (fn [cmd]
           (match cmd
                  ["noop"] [:noop]
                  ["addx" v] [:addx (read-string v)])))))

(comment
  (input "input.txt")
  (input "example.txt")
  (input "example2.txt"))


(def initial-state {:x                 1
                    :stored-cycle-vals {}
                    :instruction       nil
                    :rest-instructions []})

(defn set-next-instruction [state]
  (-> state
      (assoc :instruction (first (:rest-instructions state)))
      (update :rest-instructions rest))
  )

(defn run-program
  ([prog ticks] (run-program prog ticks nil))
  ([prog ticks opts]
   (let [store-cycle-num? (:store-cycle-num? opts)]
     (->> (range 2 (inc ticks))
          (reduce
            (fn [state cycle]
              (println cycle (-> state (dissoc :rest-instructions :stored-cycle-vals)))
              (let [state
                    (if (and (seq store-cycle-num?) (store-cycle-num? cycle))
                      (-> state (update :stored-cycle-vals assoc cycle (:x state)))
                      state)
                    current-instruction (:instruction state)]

                (match current-instruction
                       [:noop] (-> state set-next-instruction)
                       [:addx v] (-> state (dissoc :instruction) (update :x + v))
                       nil (-> state set-next-instruction))))
            (-> initial-state
                (assoc :rest-instructions prog)
                set-next-instruction))))))

(defn stored-signal-strengths [state]
  (->> state :stored-cycle-vals (map #(apply * %)) (reduce +)))

(comment
  (run-program (input "example.txt") 6 {:store-cycle-num? #{2 4 6}})

  (run-program
    (input "example2.txt") 105
    {:store-cycle-num? #{20 60 100 140 180 220}})

  (run-program
    (input "example2.txt") 220
    {:store-cycle-num? #{20 60 100 140 180 220}})

  (run-program
    (input "input.txt") 220
    {:store-cycle-num? #{20 60 100 140 180 220}})

  (stored-signal-strengths *1)

  (first (input "example.txt"))
  (first (rest (input "example.txt")))
  )

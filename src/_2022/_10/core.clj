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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sprite-char [cycle x]
  (let [sprite-x   (-> x (mod 40))
        in-sprite? #{sprite-x (inc sprite-x) (inc (inc sprite-x))}
        mod-cycle  (mod cycle 40)]
    (if (in-sprite? mod-cycle) "#" ".")))

(comment
  (mod 1 40)
  (mod 41 40)

  (sprite-char 1 1)
  (sprite-char 2 1)
  (sprite-char 3 1)
  (sprite-char 4 1)
  (sprite-char (+ 7 40) 5)
  (sprite-char (+ 17 40 40) (+ 16 40)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def initial-state
  {:x                 1
   :stored-cycle-vals {}
   :instruction       nil
   :rest-instructions []
   :img               ""})

(defn set-next-instruction [state]
  (-> state
      (assoc :instruction (first (:rest-instructions state)))
      (update :rest-instructions rest)))

(defn run-program
  ([prog ticks] (run-program prog ticks nil))
  ([prog ticks opts]
   (let [store-cycle-num? (:store-cycle-num? opts)]
     (->> (range 2 (inc ticks))
          (reduce
            (fn [state cycle]
              (let [state
                    (if (and (seq store-cycle-num?) (store-cycle-num? cycle))
                      (-> state (update :stored-cycle-vals assoc cycle (:x state)))
                      state)

                    state (update state :img str (sprite-char cycle (:x state)))

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

(defn print-img [{:keys [img]}]
  (->> img (partition 40) (map #(apply str %)) (map println)))

(comment
  (run-program (input "example.txt") 6 {:store-cycle-num? #{2 4 6}})

  (->
    (run-program
      (input "example2.txt") 400
      {:store-cycle-num? #{20 60 100 140 180 220}})
    print-img)

  (->
    (run-program
      (input "input.txt") 600)
    print-img)
  ;; somehow dropping the first # on each, but it works
  ;; PLGFKAZG

  (stored-signal-strengths *1)
  (print-img *1)

  (first (input "example.txt"))
  (first (rest (input "example.txt"))))

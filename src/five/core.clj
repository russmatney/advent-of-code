(ns five.core
  (:require [util :refer [input]]
            [clojure.set :as set])
  )

(defn pass->partition [p {:keys [upper lower size]}]
  (loop [remaining p
         parts     (range 0 size)]
    (let [mid-nth (/ (count parts) 2)
          char    (first remaining)]
      (if
          (= 1 (count parts)) (first parts)

          (recur
            (rest remaining)
            (apply range
                   (cond (= char lower)
                         [(first parts) (nth parts mid-nth)]
                         (= char upper)
                         [(nth parts mid-nth) (+ (last parts) 1)])))))))

(comment
  (/ (count (range 0 128)) 2)

  (nth (range 0 128) 64)
  (last (range 0 8))

  (/ 128 2)

  (pass->partition "FFF" {:upper \B :lower \F :size 8})
  (pass->partition "FFB" {:upper \B :lower \F :size 8})
  (pass->partition "FBF" {:upper \B :lower \F :size 8})
  (pass->partition "FBB" {:upper \B :lower \F :size 8})
  (pass->partition "BFF" {:upper \B :lower \F :size 8})
  (pass->partition "BFB" {:upper \B :lower \F :size 8})
  (pass->partition "BBF" {:upper \B :lower \F :size 8})
  (pass->partition "BBB" {:upper \B :lower \F :size 8})
  )


(defn pass->id [p]
  (let [row  (pass->partition (take 7 p) {:upper \B :lower \F :size 128})
        seat (pass->partition (drop 7 p) {:upper \R :lower \L :size 8})]
    (+ (* 8 row) seat)))

(comment
  (->> ["BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"]
       (map pass->id)
       ;; (apply max)
       )
  (->> (input "input.txt")
       (map pass->id)
       (apply max)
       )
  )

(defn missing-ids [passes]
  (let [pass-ids  (->> passes (map pass->id) sort)
        [f l]     [(first pass-ids) (last pass-ids)]
        all-seats (range f (+ l 1))]
    (set/difference (set all-seats) (set pass-ids)))
  )


(comment
  (-> ["BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"]
      (missing-ids)
      )

  (-> (input "input.txt")
      (missing-ids)))

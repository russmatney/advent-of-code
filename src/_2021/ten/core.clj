(ns _2021.ten.core
  (:require [util :refer [input]]
            [clojure.set :as set]))

(def closers->score
  {\) 3
   \} 1197
   \] 57
   \> 25137})

(def closers->openers
  {\) \(
   \} \{
   \] \[
   \> \<})

(def openers->closers (set/map-invert closers->openers))

(def is-opener? #{\( \{ \[ \<})

(defn corrupted-by [line]
  (loop [seen-openers []
         line         line]
    (if-not (seq line)
      nil
      (let [next (first line)
            rst  (rest line)]
        ;; (println "seen-openers" seen-openers)
        (cond
          (is-opener? next)
          (recur (cons next seen-openers) rst)

          ;; is-closer?
          (closers->openers next)
          (let [matching-opener (closers->openers next)]
            (if (#{matching-opener} (first seen-openers))
              (recur (rest seen-openers) rst)
              ;; corrupted!
              next)))))))

(comment
  (corrupted-by "{([(<{}[<>[]}>{[]{[(<()>")

  (->>
    (input "input.txt")
    (filter corrupted-by)
    (map corrupted-by)
    (map closers->score)
    (apply +)))


;; part 2

;; same function as above, just return the openers instead of nil
(defn unclosed-openers [line]
  (loop [seen-openers []
         line         line]
    (if-not (seq line)
      seen-openers
      (let [next (first line)
            rst  (rest line)]
        (cond
          (is-opener? next)
          (recur (cons next seen-openers) rst)

          ;; is-closer?
          (closers->openers next)
          (let [matching-opener (closers->openers next)]
            (if (#{matching-opener} (first seen-openers))
              (recur (rest seen-openers) rst)
              ;; corrupted!
              next)))))))

(def closers->score2
  {\) 1
   \} 3
   \] 2
   \> 4})

(defn to-score [closers]
  (reduce
    (fn [score closer]
      (+ (* 5 score) (closers->score2 closer)))
    0 closers))

(comment
  (let [scores
        (->>
          "input.txt"
          input
          (remove corrupted-by)
          (map unclosed-openers)
          (map (fn [ops] (map openers->closers ops)))
          (map to-score)
          sort
          vec)
        middle-n (/ (dec (count scores)) 2)]
    (nth scores middle-n)))
